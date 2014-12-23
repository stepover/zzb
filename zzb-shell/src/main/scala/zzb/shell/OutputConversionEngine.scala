package zzb.shell

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-13
 * Time: 下午4:04
 * Copyright baoxian.com 2012~2020
 */
class OutputConversionEngine {

  type OutputConverter = AnyRef ⇒ AnyRef //Function1[AnyRef,AnyRef]

  private val outputConverters = scala.collection.mutable.ListBuffer[OutputConverter]()

  def addConverter(converter: OutputConverter) {
    if (converter == null) {
      throw new IllegalArgumentException("Converter == null")
    }
    outputConverters += converter

  }

  def removeConverter(converter: OutputConverter): Boolean = {
    val has = outputConverters.contains(converter)

    if (has) outputConverters -= converter

    has
  }

  def convertOutput(anObject: AnyRef): AnyRef = {
    var convertedOutput: AnyRef = anObject
    outputConverters.toList.reverse.foreach { converter ⇒
      val conversionResult = converter.apply(convertedOutput)
      if (conversionResult != null)
        convertedOutput = conversionResult
    }

    convertedOutput
  }

  //  def addDeclaredConverters(handler: AnyRef) {
  //    val fields: Array[Field] = handler.getClass.getFields
  //    val PREFIX: String = "CLI_OUTPUT_CONVERTERS"
  //    for (field <- fields) {
  //      if (field.getName.startsWith(PREFIX) && field.getType.isArray && classOf[OutputConverter].isAssignableFrom(field.getType.getComponentType)) {
  //        try {
  //          val convertersArray: AnyRef = field.get(handler)
  //          {
  //            var i: Int = 0
  //            while (i < Array.getLength(convertersArray)) {
  //              {
  //                addConverter(Array.get(convertersArray, i).asInstanceOf[OutputConverter])
  //              }
  //              ({
  //                i += 1; i - 1
  //              })
  //            }
  //          }
  //        }
  //        catch {
  //          case ex: Exception => {
  //            throw new RuntimeException("Error getting converter from field " + field.getName, ex)
  //          }
  //        }
  //      }
  //    }
  //  }
}

