use boilermates::boilermates;

#[boilermates("CreateUser")]
#[boilermates(attr_for("CreateUser"))]
#[derive(Debug, Clone)]
pub struct User {
    #[boilermates(not_in("CreateUser"))]
    pub id: u64,
    pub name: String,
    pub email: String,
}

#[boilermates("CreateBallsUser")]
#[derive(Debug, Clone)]
pub struct BallsUser {
    #[boilermates(not_in("CreateBallsUser"))]
    pub id: u64,
    pub name: String,
    pub email: String,
}
