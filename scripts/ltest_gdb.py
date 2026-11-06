"""GDB inspection of LTest logical threads (source this file inside GDB)."""

import gdb
import gdb.unwinder


def _records():
    """Read only inferior memory; never call into a stopped target."""
    try:
        version_symbol = gdb.lookup_global_symbol("ltest::debug_abi_version")
        threads_symbol = gdb.lookup_global_symbol("ltest::debug_threads")
        if version_symbol is None or threads_symbol is None:
            return []
        # Expression parsing may request the current frame and recursively
        # invoke this unwinder, especially during 'thread apply all bt'.
        version = int(version_symbol.value())
        head = threads_symbol.value()
    except gdb.error:
        return []
    if version != 1:
        raise gdb.GdbError("Unsupported LTest debug ABI: {}".format(version))
    records = []
    seen = set()
    while int(head):
        address = int(head)
        if address in seen:
            raise gdb.GdbError("Cycle in LTest debug thread records")
        seen.add(address)
        record = head.dereference()
        records.append(record)
        head = record["next"]
    return records


def _state(record):
    if bool(record["running"]):
        return "running"
    if int(record["fiber"]):
        return "suspended"
    return "native" if _native_thread(int(record["donor_tid"])) else "exited"


def _task_name(record):
    if not int(record["task_name"]):
        return "-"
    return record["task_name"].string(length=int(record["task_name_size"]))


def _native_thread(tid):
    return next(
        (thread for thread in gdb.selected_inferior().threads()
         if thread.ptid[1] == tid),
        None,
    )


def _inspection_thread(record):
    tid = (gdb.lookup_global_symbol("ltest::debug_executor_tid").value()
           if bool(record["running"]) else record["donor_tid"])
    return _native_thread(int(tid))


def _suspended_record():
    thread = gdb.selected_thread()
    if thread is None:
        return None
    for record in _records():
        if (int(record["donor_tid"]) == thread.ptid[1]
                and not bool(record["running"])
                and int(record["fiber"])):
            return record
    return None


class LTestUnwinder(gdb.unwinder.Unwinder):
    def __init__(self):
        super().__init__("ltest")

    def __call__(self, pending_frame):
        if pending_frame.level() != 0:
            return None
        record = _suspended_record()
        if record is None:
            return None
        if pending_frame.architecture().name() != "i386:x86-64":
            return None

        # Private LTest Boost.Context, x86-64 SysV LP64, including the FS-base
        # slot. Keep in sync with third_party/boost_context's jump assembly.
        # Ordinary application Boost.Context fibers have a different layout.
        context = record["fiber"].dereference()["fctx_"]
        if not int(context):
            return None
        address = int(context)
        register_type = pending_frame.read_register("rsp").type
        saved = context.cast(register_type.pointer())

        # This is a bridge from the donor's innermost frame to another stack.
        # Use the saved context as its frame ID so normal stack-order checks
        # also work when the fiber's mapping is below the donor's native stack.
        class FrameId:
            # GDB 13 requires gdb.Value here; newer GDB also accepts integers.
            sp = gdb.Value(address)
            pc = pending_frame.read_register("rip")

        unwind = pending_frame.create_unwind_info(FrameId())
        # Words 0 and 1 hold FP control and FS base; words 2..8 are registers.
        for index, name in enumerate(("r12", "r13", "r14", "r15", "rbx", "rbp", "rip"), 2):
            unwind.add_saved_register(name, saved[index])
        unwind.add_saved_register("rsp", gdb.Value(address + 0x48).cast(register_type))
        return unwind


def _internal_frame(frame):
    name = frame.name() or ""
    return (name in ("CoroYield", "CoroutineStatusChange")
            or name.startswith(("boost::context::", "ltest::DonorTls::")))


class LTestFrameFilter:
    name = "ltest"
    priority = 100
    enabled = True

    def filter(self, frames):
        if not _records():
            yield from frames
            return
        bridge = _unwinder.enabled and _suspended_record() is not None
        for decorated in frames:
            frame = decorated.inferior_frame()
            # Task entry is the bottom of the logical stack.
            if (frame.name() or "").startswith("ltest::Coro<"):
                return
            if bridge and frame.level() == 0:
                continue
            if not _internal_frame(frame):
                yield decorated


class LTestThreads(gdb.Command):
    """List LTest logical threads and the native threads used to inspect them."""

    def __init__(self):
        super().__init__("ltest threads", gdb.COMMAND_STATUS)

    def invoke(self, argument, from_tty):
        if argument.strip():
            raise gdb.GdbError("Usage: ltest threads")
        records = sorted(_records(), key=lambda r: int(r["logical_id"]))
        if not records:
            gdb.write("No LTest logical threads in this inferior.\n")
            return
        gdb.write("Logical  GDB thread  Donor TID  State       Task\n")
        for record in records:
            thread = _inspection_thread(record)
            number = str(thread.num) if thread is not None else "-"
            gdb.write("{:<9}{:<12}{:<11}{:<12}{}\n".format(
                int(record["logical_id"]), number, int(record["donor_tid"]),
                _state(record), _task_name(record)))


class LTestThread(gdb.Command):
    """Select a logical thread's stack: ltest thread LOGICAL_ID."""

    def __init__(self):
        super().__init__("ltest thread", gdb.COMMAND_STACK)

    def invoke(self, argument, from_tty):
        try:
            logical_id = int(argument.strip(), 10)
        except ValueError:
            raise gdb.GdbError("Usage: ltest thread LOGICAL_ID") from None
        record = next((r for r in _records()
                       if int(r["logical_id"]) == logical_id), None)
        if record is None:
            raise gdb.GdbError("Unknown LTest logical thread {}".format(logical_id))
        thread = _inspection_thread(record)
        if thread is None:
            raise gdb.GdbError("Logical thread {} has no live native thread ({})"
                               .format(logical_id, _state(record)))
        thread.switch()
        frame = gdb.newest_frame()
        if _unwinder.enabled and _state(record) == "suspended":
            frame = frame.older()
        while frame is not None and _internal_frame(frame):
            frame = frame.older()
        if frame is not None:
            frame.select()
            gdb.execute("frame", from_tty=from_tty)


_unwinder = LTestUnwinder()
gdb.unwinder.register_unwinder(None, _unwinder, replace=True)
gdb.frame_filters["ltest"] = LTestFrameFilter()
gdb.Command("ltest", gdb.COMMAND_STATUS, prefix=True)
LTestThreads()
LTestThread()
gdb.write("LTest inspection loaded. Use 'ltest threads' and 'ltest thread ID'.\n")
