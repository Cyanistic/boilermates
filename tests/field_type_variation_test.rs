use boilermates::boilermates;

#[test]
fn test_same_field_different_types() {
    // This test demonstrates the issue: we want the same field name
    // but with different types in different structs
    
    #[boilermates("CreateForm")]
    #[derive(Debug, Clone)]
    struct Model {
        id: u64,
        
        // In Model, this field is required (String)
        #[boilermates(not_in("CreateForm"))]
        description: String,
        
        // In CreateForm, we want this to be optional (Option<String>)
        #[boilermates(only_in("CreateForm"))]
        description: Option<String>,
        
        name: String,
    }
    
    // Model should have required description
    let model = Model {
        id: 1,
        description: "Required description".to_string(),
        name: "Test".to_string(),
    };
    
    // CreateForm should have optional description and id
    let create_form = CreateForm {
        id: 2,
        description: Some("Optional description".to_string()),
        name: "Test".to_string(),
    };
    
    assert_eq!(model.description, "Required description");
    assert_eq!(create_form.description, Some("Optional description".to_string()));
}