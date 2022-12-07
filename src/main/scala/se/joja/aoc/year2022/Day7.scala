package se.joja.aoc.year2022

import se.joja.aoc.getInput

object Day7 extends App {

  val input = getInput("2022/day7.txt")

  val CmdCd   = """\$ cd (\w+|/|\.{2})""".r
  val CmdLs   = """\$ ls""".r
  val OutDir  = """dir (\w+)""".r
  val OutFile = """(\d+) ([\w.]+)""".r

  trait FileSystem {
    def addDir(path: List[String], dirName: String): Dir
    def addFile(path: List[String], fileName: String, fileSize: Long): Dir
    def print(indent: Int = 0): String
    def dirSizes: (Long, List[Long])
  }

  case class Dir(name: String, dirs: List[Dir] = List.empty, files: List[File] = List.empty, size: Long = 0)
      extends FileSystem {

    override def addDir(path: List[String], dirName: String): Dir = {
      path match {
        case p :: Nil if p == name =>
          if (dirs.exists(_.name == dirName)) this else this.copy(dirs = dirs :+ Dir(dirName))
        case p :: d :: _ if p == name =>
          dirs.find(_.name == d) match {
            case Some(dir) =>
              val newDirs = dirs.filterNot(_.name == dir.name) :+ dir.addDir(path.drop(1), dirName)
              this.copy(dirs = newDirs)
            case None => this.copy(dirs = dirs :+ Dir(dirName))
          }
        case _ => this
      }
    }

    override def addFile(path: List[String], fileName: String, fileSize: Long): Dir = {
      path match {
        case p :: Nil if p == name =>
          if (files.exists(_.name == fileName)) this else this.copy(files = files :+ File(fileName, fileSize))
        case p :: d :: _ if p == name =>
          dirs.find(_.name == d) match {
            case Some(dir) =>
              val newDirs = dirs.filterNot(_.name == dir.name) :+ dir.addFile(path.drop(1), fileName, fileSize)
              this.copy(dirs = newDirs)
            case None => throw new RuntimeException(s"Can't add file '$fileName' on path '$path'")
          }
        case _ => this
      }
    }

    override def print(indent: Int): String =
      " ".repeat(indent) + s"- $name (dir, size: ${dirSizes._1})\n" +
        dirs.sortBy(_.name).map(_.print(indent + 2)).mkString +
        files.sortBy(_.name).map(_.print(indent + 2)).mkString

    override def dirSizes: (Long, List[Long]) = {
      val childSize = dirs.map(_.dirSizes._1).sum
      val allChildSizes = dirs.flatMap(_.dirSizes._2)
      val mysize = childSize + files.map(_.size).sum
      (mysize, allChildSizes :+ mysize)
    }
  }

  case class File(name: String, size: Long) extends FileSystem {
    override def addDir(path: List[String], dirName: String): Dir                   = ???
    override def addFile(path: List[String], fileName: String, fileSize: Long): Dir = ???
    override def dirSizes: (Long, List[Long]) = ???

    override def print(indent: Int): String =
      " ".repeat(indent) + s"- $name (file, size=$size)\n"
  }

  val (endPath, fs) = input.foldLeft((List.empty[String], Dir("/"))) {
    case ((path, fs), line) =>
      val (newPath, newFs) = line match {
        case CmdCd("/")                  => (List("/"), fs)
        case CmdCd("..")                 => (path.dropRight(1), fs)
        case CmdCd(dirName)              => (path :+ dirName, fs)
        case CmdLs()                     => (path, fs)
        case OutDir(dirName)             => (path, fs.addDir(path, dirName))
        case OutFile(fileSize, fileName) => (path, fs.addFile(path, fileName, fileSize.toLong))
      }
      (newPath, newFs)
  }

  println(fs.print())
  println(fs.dirSizes._2.sorted)

  val result1 = fs.dirSizes._2.filter(_ <= 100000).sum
  println(s"Result part 1: $result1")

  // Part 2

  val totalSize = fs.dirSizes._2.max
  val needToDelete = totalSize - 40000000
  println(s"Need to delete: $needToDelete")
  val result2 = fs.dirSizes._2.filter(_ >= needToDelete).min

  println(s"Result part 2: $result2")
}
