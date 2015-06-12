refriedSonatype

monocle

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)
git.useGitDescribe := true

def specs2(module: String) = "org.specs2" %% s"specs2-$module" % "3.6.1" % "test"

libraryDependencies ++= Seq("core").map(specs2)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.4" % "test" force()
libraryDependencies += "org.typelevel" %% "scalaz-specs2" % "0.4.0" % "test"
libraryDependencies += scalaz("scalacheck-binding") % "test"
