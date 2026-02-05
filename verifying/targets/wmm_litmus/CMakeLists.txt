set(WMM_TARGET_LIST)

function(add_wmm_litmus name expect_fail)
  set(source_name "${name}.cpp")
  verify_target(${name})
  list(APPEND WMM_TARGET_LIST ${name})
  set(WMM_TARGET_LIST ${WMM_TARGET_LIST} PARENT_SCOPE)
  add_integration_test(${name} "litmus" ${expect_fail}
    ${name} --wmm_enabled --rounds 0 --exploration_runs 1000 --strategy random ${ARGN}
  )
  set_tests_properties("litmus_${name}"
    PROPERTIES LABELS "litmus;wmm"
  )
endfunction()

add_wmm_litmus(exp1_sc FALSE)
add_wmm_litmus(exp2_relaxed_sc FALSE)
add_wmm_litmus(exp3_sc FALSE)
add_wmm_litmus(exp4_relaxed TRUE)
add_wmm_litmus(exp5_ra FALSE)
# add_wmm_litmus(exp6_mixed TRUE) # TODO: requires a fix, see comment in the file
add_wmm_litmus(exp7_rmw_sc FALSE)
add_wmm_litmus(exp8_rmw_sc FALSE)
add_wmm_litmus(exp9_weak_rmw TRUE)

add_custom_target(verify-litmus
  DEPENDS
    ${WMM_TARGET_LIST}
)
