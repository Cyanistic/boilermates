use boilermates::boilermates;

#[test]
fn test_only_in_excludes_from_parent() {
    #[boilermates("CreateProduct", "UpdateProduct")]
    #[derive(Debug, Clone)]
    struct Product {
        id: u64,
        name: String,
        #[boilermates(only_in("CreateProduct"))]
        initial_price: f64,
        #[boilermates(only_in("UpdateProduct"))]
        new_price: f64,
    }

    // CreateProduct should have id, name, and initial_price (but NOT new_price)
    let create_product = CreateProduct {
        id: 1,
        name: "Widget".to_string(),
        initial_price: 99.99,
    };

    // UpdateProduct should have id, name, and new_price (but NOT initial_price)
    let update_product = UpdateProduct {
        id: 1,
        name: "Widget".to_string(),
        new_price: 89.99,
    };

    // Product should have id and name ONLY (neither initial_price nor new_price)
    let product = Product {
        id: 1,
        name: "Widget".to_string(),
    };

    // Verify the structs have the correct fields by using them
    assert_eq!(create_product.id, 1);
    assert_eq!(create_product.initial_price, 99.99);
    assert_eq!(update_product.id, 1); 
    assert_eq!(update_product.new_price, 89.99);
    assert_eq!(product.id, 1);
    assert_eq!(product.name, "Widget");
}

#[test] 
fn test_only_in_with_parent_included() {
    #[boilermates("ViewProduct")]
    #[derive(Debug, Clone)]
    struct ProductV2 {
        id: u64,
        name: String,
        #[boilermates(only_in("ProductV2", "ViewProduct"))]
        description: String,
        #[boilermates(only_in("ViewProduct"))]
        view_count: u32,
    }

    // ViewProduct should have all fields including view_count
    let view_product = ViewProduct {
        id: 1,
        name: "Widget".to_string(),
        description: "A great widget".to_string(),
        view_count: 42,
    };

    // ProductV2 should have id, name, and description (but NOT view_count)
    let product_v2 = ProductV2 {
        id: 1,
        name: "Widget".to_string(),
        description: "A great widget".to_string(),
    };

    // Verify the structs have the correct fields
    assert_eq!(view_product.id, 1);
    assert_eq!(view_product.view_count, 42);
    assert_eq!(product_v2.id, 1);
    assert_eq!(product_v2.description, "A great widget");
}

#[test]
fn test_only_in_self() {
    #[boilermates("PublicProduct")]
    #[derive(Debug, Clone)]
    struct InternalProduct {
        id: u64,
        name: String,
        #[boilermates(only_in_self)]
        internal_notes: String,
    }

    // PublicProduct should NOT have internal_notes
    let public_product = PublicProduct {
        id: 1,
        name: "Widget".to_string(),
    };

    // InternalProduct should have internal_notes
    let internal_product = InternalProduct {
        id: 1,
        name: "Widget".to_string(),
        internal_notes: "For internal use only".to_string(),
    };

    // Test conversion
    let converted: PublicProduct = internal_product.into();
    assert_eq!(converted.id, 1);
    assert_eq!(converted.name, "Widget");
}