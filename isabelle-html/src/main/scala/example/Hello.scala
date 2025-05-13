package example

import java.io.File
import java.nio.file.{Files, Paths}

import isabelle.Document.Node
import isabelle.Exn.ERROR
import isabelle.Prover.Receiver
import isabelle.{Bash, Document, Isabelle_Process, Isabelle_System, ML_Process, Options, Path, Prover, Resources, Session, Sessions, System_Channel, XML}

object Hello {


  val dir = "/home/peter/work/isabelle-by-example"
  val isabelleRoot = "/home/peter/apps/Isabelle2019"

  def main(args: Array[String]): Unit = {
    println("Hello world")

    System.setProperty("isabelle.root", isabelleRoot)
    val options = Options.init()
    //    val options = Options.empty + ("editor_syslog_limit", "100")
    val globalTheories = Map[String, String]()
    val imports_base: Sessions.Base = Sessions.Base.bootstrap(globalTheories)
    val resources: Resources = new Resources(imports_base)
    val session = new MySession(options, resources)

    session.all_messages.+=(Session.Consumer[Prover.Message]("myConsuer")((msg: Prover.Message) => {
      println(s"Received message: $msg")
    }))

    val xml_cache: XML.Cache = XML.make_cache()
    val channel: System_Channel = System_Channel()
    val process: Bash.Process = bashProcess(options, channel)

    val t = new Thread(isaProcess)
    t.start()

    session.start(r => {
      println(s"Started with receiver: $r")
      receiver(xml_cache, channel, process, r)
    })


    Thread.sleep(30000)
    println("Starting request ...")

    val documentPath = s"$dir/test.thy"
    val content = Files.readAllBytes(Paths.get(documentPath))

    val nodeName: Node.Name = Document.Node.Name.apply("test", dir, "test")
    val blob: Document.Blob = Document.Blob.apply(isabelle.Bytes.apply(content), documentPath, isabelle.Symbol.Text_Chunk("blub"), true)
    val edits: List[Document.Edit_Text] = List(
      nodeName -> Node.Blob(blob)
    )
    session.update(Document.Blobs.apply(Map[Document.Node.Name, Document.Blob](
      nodeName -> blob
    )), edits)


    t.join()

  }

  private def bashProcess(options: Options, channel: System_Channel): Bash.Process = {
    val process: Bash.Process =
      try {
        val channel_options =
          options.string.update("system_channel_address", channel.address).
            string.update("system_channel_password", channel.password)
        ML_Process(channel_options,
          logic = "HOL",
          args = List(),
          dirs = List(Path.explode(dir)),
          modes = List(),
          cwd = new File(dir),
          env = Map(
            "ML_HOME" -> s"$isabelleRoot/contrib/polyml-5.8/x86_64-linux/"
          ),
          sessions_structure = None,
          store = None)
      }
      catch {
        case exn@ERROR(_) => channel.shutdown(); throw exn
      }
    process.stdin.close
    process
  }

  def receiver(xml_cache: XML.Cache,
    channel: System_Channel,
    process: Bash.Process, receiver: Receiver): Prover = new Prover(receiver, xml_cache, channel, process)


  val isaProcess: Runnable = () => {

    Thread.sleep(60000)

  }

  //  def start(session: Session,
  //    options: Options,
  //    logic: String = "",
  //    args: List[String] = Nil,
  //    dirs: List[Path] = Nil,
  //    modes: List[String] = Nil,
  //    cwd: java.io.File = null,
  //    env: Map[String, String] = Isabelle_System.settings(),
  //    sessions_structure: Option[Sessions.Structure] = None,
  //    store: Option[Sessions.Store] = None,
  //    phase_changed: Session.Phase => Unit = null) {
  //    if (phase_changed != null)
  //      session.phase_changed += Session.Consumer("Isabelle_Process")(phase_changed)
  //
  //    session.start(receiver =>
  //      Isabelle_Process(options, logic = logic, args = args, dirs = dirs, modes = modes,
  //        cwd = cwd, env = env, receiver = receiver, xml_cache = session.xml_cache,
  //        sessions_structure = sessions_structure, store = store))
  //  }
}

