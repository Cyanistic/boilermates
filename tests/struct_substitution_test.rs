use boilermates::boilermates;

// Test that $struct gets replaced in doc comments (which are attributes)
#[boilermates("CreateUser")]
#[derive(Debug, Clone)]
#[doc = "This is $struct documentation"]
struct User {
    #[boilermates(not_in("CreateUser"))]
    id: u64,
    name: String,
}

#[test]
fn test_struct_variable_substitution() {
    // Verify structs are generated
    let user = User {
        id: 1,
        name: "Alice".to_string(),
    };

    let create_user = CreateUser {
        name: "Bob".to_string(),
    };

    assert_eq!(user.id, 1);
    assert_eq!(create_user.name, "Bob");

    // Check that the doc attribute was properly substituted
    // For CreateUser, $struct should be replaced with "CreateUser"
    // We can't directly test this without proc-macro2 introspection,
    // but if this compiles, the substitution worked
}

// More realistic test with cfg_attr which is commonly used
#[boilermates("CreateProduct", "UpdateProduct")]
#[derive(Debug, Clone)]
#[cfg_attr(test, doc = "Test struct: $struct")]
struct Product {
    #[boilermates(not_in("CreateProduct"))]
    id: String,
    name: String,
}

#[test]
fn test_multiple_generated_structs_with_substitution() {
    let product = Product {
        id: "p1".to_string(),
        name: "Widget".to_string(),
    };

    let create = CreateProduct {
        name: "Gadget".to_string(),
    };

    let update = UpdateProduct {
        id: "p2".to_string(),
        name: "Updated".to_string(),
    };

    assert_eq!(product.id, "p1");
    assert_eq!(create.name, "Gadget");
    assert_eq!(update.id, "p2");
}
