// RUN: %check
#include <coroutine>
struct SimpleAwaitable {
    bool await_ready() const noexcept { 
        return false; 
    }
    
    bool await_suspend(std::coroutine_handle<> h) const noexcept {
        h.resume(); 
        return false;
    }

    void await_resume() const noexcept {
    }
};

struct CoroTask {
    struct promise_type {
        CoroTask get_return_object() { return {}; }
        std::suspend_never initial_suspend() { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }
        void return_void() {}
        void unhandled_exception() {}
    };
    auto operator co_await() const { return SimpleAwaitable{}; }

};

CoroTask myCoroutine2() {
    co_return;
}

// CHECK: call
CoroTask myCoroutine() {    
    co_await myCoroutine2();
}