import ChromeManifest.{Background, Oauth2Settings, Overrides}

case class ChromeManifest(name: String,
                          key: String,
                          version: String,
                          manifestVersion: Int,
                          background: Background,
                          oauth2: Oauth2Settings,
                          //                                 description: Option[String] = None,
                          offlineEnabled: Boolean,
                          permissions: Set[String],
                          webAccessibleResources: Set[String],
                          //                                 icons: Map[Int, String] = Map(),
                          chromeUrlOverrides: Overrides)

object ChromeManifest {
  case class Background(scripts: List[String])

  case class Oauth2Settings(clientId: String, scopes: List[String])

  case class Overrides(newtab: String)

  implicit val pkl: upickle.default.ReadWriter[ChromeManifest] =
    upickle.default.macroRW[ChromeManifest]

  val mySettings = ChromeManifest(
    name = "Slate",
    key = "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4N4oLgAv/KqcR8JWbDiC8O0l5B0/qivL89U3RKiJQkhBO0CQBqY3aqVvq5z9lX1HHei6Hbk4i4T4sUZoXJ+89c8zxwuz72temzebYbOFa9wMWedbvbEK51fdOrOI6GylMVkxIy+D2ZvI4tAAgcLmoEYOSvNnLspKGGS5F18KokrizLqgM3nuEQ/XVzJ5VxpA9LtxFlh8aawpRl1cfJ1yey686v245YJCeXLwNo7eq3PkVQ6maOYCugB/yWtxx9pfX5/SJAuPfOzttNBpv3CKLZK+RPKblgN+xJF05DaCwvIbconrd46VSRrNw3v+GgGXFlTk8CzbYKhboGSj9dJjiwIDAQAB",
    version = "0.0.1",
    manifestVersion = 2,
    oauth2 = Oauth2Settings(
      clientId = "475975148140-onsakb7o5ht32neb1m5belmp9tg756va.apps.googleusercontent.com",
      scopes = List(
        "https://mail.google.com/",
        "https://mail.google.com/",
        "https://www.googleapis.com/auth/calendar.readonly",
        "https://www.googleapis.com/auth/calendar"
      )
    ),
    background = Background(List("deps.js", "main.js", "launcher.js")),
    offlineEnabled = true,
    permissions = Set("<all_urls>", "identity"),
    webAccessibleResources = Set("fonts/akrobat/*.woff2", "fonts/sanfrancisco/*.woff", "fonts/*"),
    chromeUrlOverrides = Overrides(newtab = "index-dev.html")
  )

}
