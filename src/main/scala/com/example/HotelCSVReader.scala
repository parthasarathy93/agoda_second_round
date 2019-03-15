package com.example

import scala.io.Source

class HotelCSVReader(val fileName: String) extends HotelReader {

  override def readHotels(): Seq[Hotel] = {
    for {
      line <- Source.fromFile(fileName).getLines().drop(1).toVector
      values = line.split(",").map(_.trim)
    } yield Hotel(values(0), values(1), values(2), values(3).toInt)
  }

}