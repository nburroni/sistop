/**
  * Created by nico on 23/05/16.
  */
import javax.imageio.ImageIO
import java.awt._
import java.awt.image.BufferedImage
import java.io.File

import java.awt.Color._

case class Drawing(outputName: String, margin: Int = 50) {

  private val imgWidth = 1000
  private val imgHeight = 600
  private val height = imgHeight - 2 * margin
  private val width = imgWidth - 2 * margin

  val dashed = new BasicStroke(1.0f,
    BasicStroke.CAP_BUTT,
    BasicStroke.JOIN_MITER,
    10.0f, Array(10f), 0.0f)
  val basic = new BasicStroke(2f)

  private val img: BufferedImage = new BufferedImage(imgWidth, imgHeight, BufferedImage.TYPE_INT_ARGB)
  private val g: Graphics2D = img.createGraphics()
  private var processCount = 0
  private var processMap = Map[Int, Int]()
  private var currentX = 90

  private var clock = 0
  g.setColor(white)
  g.fillRect(0, 0, img.getWidth(), img.getHeight())
  resetColor
  verticalLine

  def verticalLine = {
    g.setStroke(dashed)
    drawLine((currentX, 0), (currentX, height), black)
    g.setStroke(basic)
    drawString(s"$clock ms", (currentX, height + 4), black)
  }
  def resetColor = setColor(black)
  def setColor(c: Color) = g.setColor(c)

  def drawLine(p0: (Int, Int), p1: (Int, Int), color: Color) = {
    setColor(color)
    g.drawLine(p0._1 + margin, p0._2 + margin, p1._1 + margin, p1._2 + margin)
    resetColor
  }
  def drawString(str: String, p: (Int, Int), color: Color) = {
    setColor(color)
    g.drawString(str, p._1 + margin, p._2 + margin + 4)
    resetColor
  }

  def addProcess(id: Int) = {
    val y = 50 * processCount
    drawString(s"Process $id", (0, y), black)
    processMap = processMap + (id -> y)
    processCount += 1
  }

  def processRun(id: Int, time: Int) = {
    val y = processMap.getOrElse(id, 0)
    val timeLength = time * 35
    drawLine((currentX, y), (currentX + timeLength, y), green)
    currentX += timeLength
    clock += time
    verticalLine
  }

  def processIO(id: Int, time: Int, clock: Int) = {
    val y = processMap.getOrElse(id, 0)
    val timeLength = time * 35
    val clockPosition = clock
    g.setStroke(dashed)
    drawLine((clockPosition, y), (clockPosition + timeLength, y), red)
    g.setStroke(basic)
  }



  def write = ImageIO.write(img, "png", new File(s"$outputName.png"))

}
