# configuration object defaults validate each constructed value

    Code
      pamk()
    Condition
      Error:
      ! <rtemis::PAMKConfig> object properties are invalid:
      - @criterion must hold rtemis::PAMKCriterionConfig objects; invalid element 1.

# schema default references are reconstructed for each instance

    Code
      cls()
    Condition
      Error:
      ! criterion must hold rtemis::PAMKCriterionConfig objects; invalid element 1.
