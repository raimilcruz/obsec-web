package models

import java.io.File
import java.nio.file.Paths

import scala.xml.{Elem, XML}
import scala.io.Source

/**
  * Class to load defined examples
  */
class ExampleHelper(val examplePath:String,fileExtension:String) {

  private def parseMetadata(file: File) = {
    val xml = XML.loadFile(file.getAbsolutePath)
    val title = (xml \\ "Title").text
    var description = (xml \\ "Description").text
    if(description == ""){
      val descriptionFile = (xml \\ "DescriptionFile").text
      description =
        if(descriptionFile != "")
          getMarkDownFileContent(descriptionFile)
        else getMarkDownFileContent(removeFileExtension(file.getName) + ".md")
    }
    var program  =  getDartProgram(getFileNameNoExtension(file.getName))
    Example(file.getName,title,description,program)
  }
  private def getMarkDownFileContent(descriptionFileName: String): String={
    val markdownFile = new File(examplePath,descriptionFileName)
    if(markdownFile.exists()){
      println(s"********************** ${markdownFile.toURI}")
      val source = Source.fromFile(markdownFile.toURI).getLines mkString "\n"
      return source
    }
    ""
  }
  private def removeFileExtension(fileName:String):String = {
    if (fileName.indexOf(".") > 0)
      return fileName.substring(0, fileName.lastIndexOf("."))
    fileName
  }

  private def getFileNameNoExtension(fileName: String) = {
    val i = fileName.lastIndexOf('.')
    if (i > 0)
      fileName.substring(0,i)
    else ""
  }
  private def getDartProgram(fileName: String) = {
    val dartFilePath = Paths.get(examplePath,fileName+fileExtension)
    Source.fromFile(dartFilePath.toString).getLines.mkString("\n")
  }

  /**
    * Gives the builtin examples.
    * @return
    */
  def examples(): List[Example]={
    var files = getListOfFiles(examplePath).sortBy(f=>f.getName)
    files.map(parseMetadata)
  }
  private def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(x=>x.isFile && x.getName.endsWith(".xml")).toList
    } else {
      List[File]()
    }
  }
}
case class Example(id:String,title:String,desc:String, program:String)
