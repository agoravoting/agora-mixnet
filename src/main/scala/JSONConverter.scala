
import play.api.libs.json._

trait JSONConverter {
  def CreatedToJS(input: Created) : JsValue = {
    /*
    Json.obj(
        "contextElements" -> Json.arr(Json.obj(
            "type" -> "Post",
            "isPattern" -> "false",
            "id" -> s"${post.board_attributes.index}",
            "attributes" -> Json.arr(Json.obj(
                "name" -> "post",
                "type" -> "Post",
                "value" -> Json.toJson(post)
            ))
        )),
        "updateAction" -> "APPEND"
    )
    */
    Json.obj(
      "id" -> input.id,
      "cSettings" -> Json.obj(
        "group" -> input.cSettings.group.getModulus().toString(),
        "generator" -> input.cSettings.generator.convertToString()
      )
    )
  }
}