import ChromeManifest.{Background, Oauth2Settings, Overrides}

case class ChromeManifest(name: String,
                          version: String,
                          manifestVersion: Int,
                          background: Background,
                          oauth2: Oauth2Settings,
                          //                                 description: Option[String] = None,
                          offlineEnabled: Boolean,
                          permissions: Set[String],
                          //                                 icons: Map[Int, String] = Map(),
                          chromeUrlOverrides: Overrides)

object ChromeManifest {
  case class Background(scripts: List[String])

  case class Oauth2Settings(clientId: String, scopes: List[String])

  case class Overrides(newtab: String)
  object Overrides {
    implicit val pkl = upickle.default.macroRW[Overrides]
  }

  implicit val pkl = upickle.default.macroRW[ChromeManifest]
  val mySettings = ChromeManifest(
    name = "Dashboarder",
    version = "0.0.1",
    manifestVersion = 2,
    oauth2 = Oauth2Settings(
      clientId = "475975148140-rndi4kl7ohdsedfpsgipj283sagvphnq.apps.googleusercontent.com",
      scopes = List(
        "https://mail.google.com/"
      )
    ),
    background = Background(List("deps.js", "main.js", "launcher.js")),
    offlineEnabled = true,
    permissions = Set("<all_urls>"),
    chromeUrlOverrides = Overrides(newtab = "index-dev.html")
  )
}
