
Designing software with types.

Some words:
- "design flexible and reusable object-oriented software"
- "that is, objects that are easier to implement, change, test, and reuse."

Domain (TLD) Structure:

- \ TLD:
```sh
Models -> DAL -> Coordinator -> Controllers
```
- \ SLD:
```hs
-- Notes:
-- () : is function
-- other else, is Type
-- 
--
-- Models
  Model
    Param
      Model Attributes
      Create Attributes
    Property
      Table Name
      Primary Key Attribute
      Primary Key Attributes
      Associations
      Options
      ...
    --
    -- DAL (database abstraction layer)
      DAL
        Param
          Model
          Model Attributes
          Model Creation Attributes
          Model Update Attributes
        Property
          Create ()
          Create Any ()
          Where ()
          Get ()
          First ()
          Update () 
          Update Any ()
          Delete () 
          Count ()
        --
        -- Coordinator
          Coordinator
            Param
              Model
              Model Attributes
              Model Creation Attributes
              Model Update Attributes
            Sub
              Create Coordinator -- Based on Param
                Create
              Read Coordinator
                Get Plain ()
                Get ()
                Get All ()
                First ()
                Where ()
                Count ()
              Update Coordinator
                Update ()
              Delete Coordinator
                Delete()
                Batch Delete By Ids ()
                Bath Delete ()
              ---
```