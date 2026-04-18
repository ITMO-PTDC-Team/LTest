set(WMM_TARGET_LIST)

function(add_wmm_litmus name expect_fail)
  set(source_name "${name}.cpp")
  verify_target(${name})
  list(APPEND WMM_TARGET_LIST ${name})
  set(WMM_TARGET_LIST ${WMM_TARGET_LIST} PARENT_SCOPE)
  add_integration_test(${name} "litmus" ${expect_fail}
    ${name} --wmm_enabled --rounds 0 --exploration_runs 10000 --strategy random ${ARGN}
  )
  set_tests_properties("litmus_${name}"
    PROPERTIES LABELS "litmus;wmm"
  )
endfunction()

add_wmm_litmus(litmus1_sc FALSE)
add_wmm_litmus(litmus2_relaxed_sc FALSE)
add_wmm_litmus(litmus3_sc FALSE)
add_wmm_litmus(litmus4_relaxed TRUE)
add_wmm_litmus(litmus5_ra FALSE)
add_wmm_litmus(litmus6_mixed TRUE) # TODO: requires a fix, see comment in the file
add_wmm_litmus(litmus7_rmw_sc FALSE)
add_wmm_litmus(litmus8_rmw_sc FALSE)
add_wmm_litmus(litmus9_weak_rmw TRUE)
add_wmm_litmus(litmus10_exchange_sc FALSE)
add_wmm_litmus(litmus11_fetchadd_weak TRUE)
add_wmm_litmus(litmus12_fetchsub_relacq FALSE)
add_wmm_litmus(litmus13_prefix_inc_sc FALSE)
add_wmm_litmus(litmus14_prefix_dec_sc FALSE)
add_wmm_litmus(litmus15_postinc_sc FALSE)
add_wmm_litmus(litmus16_fetchand_relacq FALSE)
add_wmm_litmus(litmus17_fetchor_weak TRUE)
add_wmm_litmus(litmus18_fetchxor_relacq FALSE)
add_wmm_litmus(litmus19_andassign_sc FALSE)
add_wmm_litmus(litmus20_oreq_sc FALSE)
add_wmm_litmus(litmus21_xoreq_sc FALSE)
add_wmm_litmus(litmus22_opand_weak TRUE)
add_wmm_litmus(litmus23_postdec_sc FALSE)
add_wmm_litmus(litmus24_oreq_weak TRUE)
add_wmm_litmus(litmus25_xoreq_relacq FALSE)
add_wmm_litmus(litmus26_xoreq_weak TRUE)
add_wmm_litmus(litmus27_relseq_rmw_acq FALSE)
add_wmm_litmus(litmus28_relseq_same_thread_rlx FALSE)

add_custom_target(verify-litmus
  DEPENDS
    ${WMM_TARGET_LIST}
)
