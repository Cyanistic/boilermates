use boilermates::boilermates;

#[boilermates("CreateUser")]
struct User {
    #[boilermates(not_in("CreateUser"), init = "42")]
    id: i32,

    #[boilermates(not_in("CreateUser"), init = "\"auto-generated\".to_string()")]
    status: String,

    name: String,
    email: String,
}

#[test]
fn test_init_attribute_generates_from_impl() {
    let create_user = CreateUser {
        name: "Test User".to_string(),
        email: "test@example.com".to_string(),
    };

    let user: User = create_user.into();

    assert_eq!(user.name, "Test User");
    assert_eq!(user.email, "test@example.com");
    // ID and status should be auto-initialized with the expressions
    assert_eq!(user.id, 42);
    assert_eq!(user.status, "auto-generated");
}

#[test]
fn test_init_attribute_with_complex_expression() {
    #[boilermates("CreateItem")]
    struct Item {
        #[boilermates(not_in("CreateItem"), init = "vec![1, 2, 3]")]
        numbers: Vec<i32>,

        name: String,
    }

    let create_item = CreateItem {
        name: "Test Item".to_string(),
    };

    let item: Item = create_item.into();

    assert_eq!(item.name, "Test Item");
    assert_eq!(item.numbers, vec![1, 2, 3]);
}
