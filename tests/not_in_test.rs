use boilermates::boilermates;

#[test]
fn test_not_in_excludes_from_parent() {
    #[boilermates("CreateItem")]
    #[derive(Debug, Clone)]
    struct Item {
        #[boilermates(not_in("CreateItem"))]
        id: u64,
        name: String,
        description: String,
    }

    // CreateItem should not have the id field
    let create_item = CreateItem {
        name: "Test Item".to_string(),
        description: "Test Description".to_string(),
    };

    // Item should have the id field
    let item = Item {
        id: 1,
        name: "Test Item".to_string(),
        description: "Test Description".to_string(),
    };

    // Conversion from CreateItem to Item should work
    let new_item = create_item.into__item(2);
    assert_eq!(new_item.id, 2);
    assert_eq!(new_item.name, "Test Item");
    assert_eq!(new_item.description, "Test Description");

    // Conversion from Item to CreateItem should work
    let create_from_item: CreateItem = item.into();
    assert_eq!(create_from_item.name, "Test Item");
    assert_eq!(create_from_item.description, "Test Description");
}

#[test]
fn test_only_in_works_correctly() {
    #[boilermates("UpdateItem", "ItemResponse")]
    #[derive(Debug, Clone)]
    struct ItemV2 {
        id: u64,
        name: String,
        #[boilermates(only_in("ItemV2", "ItemResponse"))]
        created_at: String,
        #[boilermates(only_in("UpdateItem"))]
        updated_at: String,
    }

    // UpdateItem should have id, name, and updated_at
    let update_item = UpdateItem {
        id: 1,
        name: "Updated".to_string(),
        updated_at: "2024-01-01".to_string(),
    };

    // ItemResponse should have id, name, and created_at
    let item_response = ItemResponse {
        id: 1,
        name: "Response".to_string(),
        created_at: "2024-01-01".to_string(),
    };

    // ItemV2 should have id, name, and created_at (but not updated_at)
    let item_v2 = ItemV2 {
        id: 1,
        name: "Item".to_string(),
        created_at: "2024-01-01".to_string(),
    };

    // Test conversions work
    let converted_item = update_item.into__item_v2("2024-01-01".to_string());
    assert_eq!(converted_item.id, 1);
    assert_eq!(converted_item.name, "Updated");
    assert_eq!(converted_item.created_at, "2024-01-01");
}