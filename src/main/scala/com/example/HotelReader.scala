package com.example

trait HotelReader {

  /**
   * @return A [[Seq]] containing all the sales.
   */
  def readHotels(): Seq[Hotel]

}

case class Hotel(city: String, hotelid: String, room: String, price: Int)
