# predict_boots() throws an error when training_data/new_data doesn't match expected format

    Code
      predict_boots(workflow = test_wf_fit, n = 1, training_data = test_train[, 3],
      new_data = test_test)
    Condition
      Error:
      ! missing cols in training_data:
      bill_depth_mm, bill_length_mm, flipper_length_mm, island, sex, species, body_mass_g

---

    Code
      predict_boots(workflow = test_wf_fit, n = 1, training_data = test_train,
        new_data = test_test[, 3])
    Condition
      Error:
      ! missing cols in new_data:
      bill_depth_mm, bill_length_mm, flipper_length_mm, island, sex, species

