use boilermates::boilermates;

#[test]
fn test_only_in_and_not_in_together() {
    // This test demonstrates that both only_in and not_in work correctly,
    // including excluding fields from the parent struct
    
    #[boilermates("CreateEntity", "UpdateEntity", "EntityView")]
    #[derive(Debug, Clone)]
    struct Entity {
        // This field is in all structs EXCEPT CreateEntity
        #[boilermates(not_in("CreateEntity"))]
        id: u64,
        
        // This field is in all structs
        name: String,
        
        // This field is ONLY in CreateEntity (not even in Entity!)
        #[boilermates(only_in("CreateEntity"))]
        initial_password: String,
        
        // This field is ONLY in UpdateEntity (not even in Entity!)
        #[boilermates(only_in("UpdateEntity"))]
        updated_by: String,
        
        // This field is ONLY in EntityView (not even in Entity!)
        #[boilermates(only_in("EntityView"))]
        view_count: u32,
        
        // This field is ONLY in Entity itself
        #[boilermates(only_in_self)]
        internal_metadata: String,
    }

    // CreateEntity has: name, initial_password (no id)
    let create_entity = CreateEntity {
        name: "Test".to_string(),
        initial_password: "secret123".to_string(),
    };
    
    // UpdateEntity has: id, name, updated_by
    let update_entity = UpdateEntity {
        id: 1,
        name: "Test".to_string(),
        updated_by: "admin".to_string(),
    };
    
    // EntityView has: id, name, view_count
    let entity_view = EntityView {
        id: 1,
        name: "Test".to_string(),
        view_count: 42,
    };
    
    // Entity has: id, name, internal_metadata (none of the only_in fields!)
    let entity = Entity {
        id: 1,
        name: "Test".to_string(),
        internal_metadata: "internal".to_string(),
    };
    
    // Verify each struct has the correct fields
    assert_eq!(create_entity.name, "Test");
    assert_eq!(create_entity.initial_password, "secret123");
    
    assert_eq!(update_entity.id, 1);
    assert_eq!(update_entity.name, "Test");
    assert_eq!(update_entity.updated_by, "admin");
    
    assert_eq!(entity_view.id, 1);
    assert_eq!(entity_view.name, "Test");
    assert_eq!(entity_view.view_count, 42);
    
    assert_eq!(entity.id, 1);
    assert_eq!(entity.name, "Test");
    assert_eq!(entity.internal_metadata, "internal");
}

#[test]
fn test_parent_struct_field_exclusion() {
    // This test specifically verifies that marking a field with not_in("ParentStruct")
    // excludes it from the parent struct
    
    #[boilermates("ChildStruct")]
    #[derive(Debug, Clone)]
    struct ParentStruct {
        // These fields should NOT be in ParentStruct
        #[boilermates(not_in("ParentStruct"))]
        excluded_from_parent: String,
        
        #[boilermates(only_in("ChildStruct"))]
        only_in_child: String,
        
        // This field should be in both
        shared_field: u32,
    }
    
    // ParentStruct should only have shared_field
    let parent = ParentStruct {
        shared_field: 42,
    };
    
    // ChildStruct should have all three fields
    let child = ChildStruct {
        excluded_from_parent: "excluded".to_string(),
        only_in_child: "child only".to_string(),
        shared_field: 42,
    };
    
    assert_eq!(parent.shared_field, 42);
    assert_eq!(child.excluded_from_parent, "excluded");
    assert_eq!(child.only_in_child, "child only");
    assert_eq!(child.shared_field, 42);
}