# configuration object defaults validate each constructed value

    Code
      pamk()
    Condition
      Error in `pamk()`:
      ! <rtemis::PAMKConfig> object properties are invalid:
      - @criterion must hold rtemis::PAMKCriterionConfig objects; invalid element 1.

# schema default references are reconstructed for each instance

    Code
      cls()
    Condition
      Error in `<DefaultReference>@criterion`:
      ! criterion must hold rtemis::PAMKCriterionConfig objects; invalid element 1.
