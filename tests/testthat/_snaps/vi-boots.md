# vi_boots() throws an error when not passed a workflow

    Code
      vi_boots(workflow = test_train, n = 1, training_data = test_train)
    Condition
      Error:
      ! argument `workflow` must be of class "workflow".

# vi_boots() throws an error when workflow is not fit

    Code
      vi_boots(workflow = test_wf_bad, n = 1, training_data = test_train)
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error:
      ! all tuning parameters must be final before passing workflow to `predict_boots()`.

# vi_boots() throws an error when bad n is specified

    Code
      vi_boots(workflow = test_wf_fit, n = 0, training_data = test_train)
    Condition
      Error:
      ! argument `n` must be >= 1.

---

    Code
      vi_boots(workflow = test_wf_fit, n = 1.5, training_data = test_train)
    Condition
      Error:
      ! argmuent `n` must be an integer.

# vi_boots() throws an error when training_data doesn't match expected format

    Code
      vi_boots(workflow = test_wf_fit, n = 1, training_data = test_train[, 3])
    Condition
      Error:
      ! missing cols in training_data:
      bill_depth_mm, bill_length_mm, flipper_length_mm, island, sex, species, body_mass_g

