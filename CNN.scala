package pplAssignment

object F2016A7PS0103P{
    //Start Coding from here
    //helper function to find dot product for a row in convolutional layer
   def row_product(matrix_1_row: List[Double],matrix_2_row: List[Double]):Double = matrix_1_row match
        {
           case Nil=>0
           case head::rest=>(matrix_1_row.head*matrix_2_row.head)+row_product(matrix_1_row.tail,matrix_2_row.tail)
        }
//finds dot product of 2 matrices    
   def dotProduct(matrix_1:List[List[Double]],matrix_2:List[List[Double]]):Double = matrix_1 match
   {
        case Nil=>0
        case head::rest=>row_product(matrix_1.head,matrix_2.head)+dotProduct(matrix_1.tail,matrix_2.tail);
   }
   
//extract number of elements = kernel size from row
def extractRow(ImageRow:List[Double],c_cnt:Int,startc:Int,kernelSize:List[Int]):List[Double] = ImageRow match
{
        case Nil=>Nil
        case head::rest=>if(startc>0)
        {
            extractRow(rest,c_cnt,startc-1,kernelSize)
        }
        else
        {
            if(c_cnt<kernelSize.tail.head)
            {
                List(head):::extractRow(rest,c_cnt+1,startc,kernelSize)
            }
            else
            {
                Nil
            }
        }
}


def extractColumn(Image:List[List[Double]],c_cnt:Int,r_cnt:Int,startr:Int,startc:Int,kernelSize:List[Int],Kernel:List[List[Double]]):List[List[Double]] = Image match
{
    case Nil=>Nil
    case head::rest=>if(startr>0)
    {
        extractColumn(rest,c_cnt,r_cnt,startr-1,startc,kernelSize,Kernel)
    }
    else
    {
        if(r_cnt<kernelSize.head)
        {
            List(extractRow(head,c_cnt,startc,kernelSize)):::extractColumn(rest,c_cnt,r_cnt+1,startr,startc,kernelSize,Kernel)
        }
        else
       {
          Nil
       }
    }
}

def extractAllColumns(Image:List[List[Double]],r_cnt:Int,c_cnt:Int,startr:Int,startc:Int,Kernel:List[List[Double]],kernelSize:List[Int],imageSize:List[Int]):List[Double] = Image match
{
    case Nil=>Nil
    case head::rest=>if(startc<imageSize.tail.head-kernelSize.tail.head+1)
    {
        List(dotProduct((extractColumn(Image,c_cnt,r_cnt,startr,startc,kernelSize,Kernel)),Kernel)):::extractAllColumns(Image,r_cnt,c_cnt,startr,startc+1,Kernel,kernelSize,imageSize)
    }
    else
    {
        Nil
    }
}


//extract all rows for pooling
//extract all columns of required size
def extractAllRows(Image:List[List[Double]],r_cnt:Int,c_cnt:Int,startr:Int,startc:Int,Kernel:List[List[Double]],kernelSize:List[Int],imageSize:List[Int]):List[List[Double]] = Image match
{
    case Nil=>Nil
    case head::rest=>if(startr<imageSize.head-kernelSize.head+1)
    {
        List(extractAllColumns(Image,r_cnt,c_cnt,startr,startc,Kernel,kernelSize,imageSize)):::extractAllRows(Image,r_cnt,c_cnt,startr+1,startc,Kernel,kernelSize,imageSize)
    }
    else
    {
        Nil
    }
}
  
//convolute
def convolute(Image:List[List[Double]],Kernel:List[List[Double]],imageSize:List[Int],kernelSize:List[Int]):List[List[Double]] = 
{
    extractAllRows(Image,0,0,0,0,Kernel,kernelSize,imageSize)
}

//extract number of elements = kernel size from row
def extractRowForPooling(ImageRow:List[Double],c:Int,start:Int,K:Int):List[Double] = ImageRow match
{
        case Nil=>Nil
        case head::rest=>if(start>0)
        {
            extractRowForPooling(rest,c,start-1,K)
        }
        else
        {
            if(c<K)
            {
                List(head):::extractRowForPooling(rest,c+1,start,K)
            }
            else
            {
                Nil
            }
        }
}
//extract all columns of required size
def extractAllColumnsForPooling(Image:List[List[Double]],r_cnt:Int,c_cnt:Int,startr:Int,startc:Int,K:Int,poolingFunc:List[Double]=>Double):List[Double] = Image match
{
    case Nil=>Nil
    case head::rest=>if(startc+K<=Image.head.length)
    {
        List(poolingFunc(extractColumnForPooling(Image,r_cnt,c_cnt,startr,startc,K,poolingFunc))):::extractAllColumnsForPooling(Image,r_cnt,c_cnt,startr,startc+K,K,poolingFunc)
    }
    else
    {
        Nil
    }
}


//extract all rows for pooling
//extract all columns of required size
def extractAllRowsForPooling(Image:List[List[Double]],r_cnt:Int,c_cnt:Int,startr:Int,startc:Int,K:Int,poolingFunc:List[Double]=>Double):List[List[Double]] = Image match
{
    case Nil=>Nil
    case head::rest=>if(startr+K<=Image.head.length)
    {
        List(extractAllColumnsForPooling(Image,r_cnt,c_cnt,startr,startc,K,poolingFunc)):::extractAllRowsForPooling(Image,r_cnt,c_cnt,startr+K,startc,K,poolingFunc)
    }
    else
    {
        Nil
    }
}
//extract for rows = kernelSize
def extractColumnForPooling(Image:List[List[Double]],r_cnt:Int,c_cnt:Int,startr:Int,startc:Int,K:Int,poolingFunc:List[Double]=>Double):List[Double] = Image match
{
    case Nil=>Nil
    case head::rest=>if(startr>0)
    {
        extractColumnForPooling(rest,r_cnt,c_cnt,startr-1,startc,K,poolingFunc)
    }
    else
    {
        if(r_cnt<K)
        {
            List(poolingFunc(extractRowForPooling(head,startr,startc,K))):::extractColumnForPooling(rest,r_cnt+1,c_cnt,startr,startc,K,poolingFunc)
        }
        else
        {
            Nil
        }
    }
}
//singlePooling
  def singlePooling(poolingFunc:List[Double]=>Double,Image:List[List[Double]],K:Int):List[Double] = 
  {
      extractAllColumnsForPooling(Image,0,0,0,0,K,poolingFunc)
  }
 
//poolingLayer using singlePooling
  def poolingLayer(poolingFunc:List[Double]=>Double,Image:List[List[Double]],K:Int):List[List[Double]] = 
 {
     extractAllRowsForPooling(Image,0,0,0,0,K,poolingFunc)
 }
 
//max Pooling function
 def maxPooling(Row:List[Double]):Double = 
 {
     max(Row)
 }
 
 
 def AvPool(Row:List[Double],presentSum:Double,size:Int):Double = Row match
 {
     case Nil=>presentSum/size
     case head::rest=>AvPool(rest,presentSum+head,size)
 }
 //write average pooling
 def avgPooling(Row:List[Double]):Double = 
 {
     val size = Row.size
     AvPool(Row,0.0,size)
 }
 
 //activation Layer for a row
  def activationRow(activationFunc:Double=>Double,Row:List[Double]):List[Double] = Row match
  {
        case Nil=>Nil
        case head::rest=>List(activationFunc(head)):::activationRow(activationFunc,rest);
  }
//activation Layer for matrix
  def activationLayer(activationFunc:Double=>Double,Image:List[List[Double]]):List[List[Double]] = Image match
  {
       case Nil=>Nil
       case head::rest=>List(activationRow(activationFunc,head)):::activationLayer(activationFunc,rest)
  }
//to be commented, custom activation,pooling function  
//   def myactivationFunc(x:Double):Double=
//   {
//      x*2.0 
//   }
  
//ReLuElement function
def ReLu(Element:Double):Double = 
{
    if(Element<0)
    {
        0
    }    
    else
    {
        Element
    }
}  
  
  
//LeakyReLuElement function
def LeakyReLu(Element:Double):Double = 
{
    if(Element<0)
    {
        0.5*Element
    }    
    else
    {
        Element
    }
}  
  
  //findmaxRow function
def max(xs: List[Double]): Double = {
    if(xs.tail.nonEmpty){
      val tl = max(xs.tail)
      if(tl > xs.head) tl
      else xs.head
    }else{
      xs.head
    }
  }
//findmax function
def findmax(xs:List[List[Double]]):Double = 
{
    if(xs.tail.nonEmpty){
      val tl = findmax(xs.tail)
      if(tl > max(xs.head)) tl
      else max(xs.head)
    }else{
      max(xs.head)
    }
}


def min(xs: List[Double]): Double = {
    if(xs.tail.nonEmpty){
      val tl = min(xs.tail)
      if(tl < xs.head) tl
      else xs.head
    }else{
      xs.head
    }
  }
//findmax function
def findmin(xs:List[List[Double]]):Double = 
{
    if(xs.tail.nonEmpty){
      val tl = findmin(xs.tail)
      if(tl < min(xs.head)) tl
      else min(xs.head)
    }else{
      min(xs.head)
    }
}
//findminRow function
def findminRow(ImageRow:List[Double],presentmin:Double):Double = ImageRow match
{
    case Nil=>presentmin
    case head::rest=>
    {
        if(head<presentmin)
        {
            findminRow(rest,head)
        }
        else
        {
            findminRow(rest,presentmin)
        }
    }
}
//findmin function
def findmin(Image:List[List[Double]],presentmin:Double):Double = Image match
{
    case Nil=>presentmin
    case head::rest=>
    {
        val tempmin = findminRow(head,presentmin)
        if(tempmin<presentmin)
        {
           findmin(rest,tempmin)
        }
        else
        {
            findmin(rest,presentmin)
        }
    }
}

//normalise Element
def normaliseElement(max:Double,min:Double,Element:Double):Int = 
{
    if(max==min)
    {
        255
    }
    else
    {
        val normalised_value = (((Element-min)/(max-min))*255)
        Math.round(normalised_value).toInt
    }
}
//normaliseRow
 def normaliseRow(max:Double,min:Double,ImageRow:List[Double]):List[Int]  = ImageRow match{
     case Nil=>Nil
     case head::rest=>List(normaliseElement(max,min,head)):::normaliseRow(max,min,rest)
       }
       
//normalisemaxmin function
def normalisemaxmin(Image:List[List[Double]],max:Double,min:Double):List[List[Int]] = Image match
{
    case Nil=>Nil
    case head::rest=>
    {
        List(normaliseRow(max,min,head)):::normalisemaxmin(rest,max,min)
    }
}
//normalise function
 def normalise(Image:List[List[Double]]):List[List[Int]] = 
 {
     val max = findmax(Image)
     val min = findmin(Image)
     normalisemaxmin(Image,max,min) 
 }
 
 def mixedLayer(Image:List[List[Double]],Kernel:List[List[Double]],imageSize:List[Int],kernelSize:List[Int],activationFunc:Double=>Double,poolingFunc:List[Double]=>Double,K:Int):List[List[Double]] = 
{
    val out = convolute(Image,Kernel,imageSize,kernelSize)
    val out2 = activationLayer(activationFunc,out)
    val out3 = poolingLayer(poolingFunc,out2,K)
    return out3
 }
 
 
//multiplierElement function
def multiplierElement(Element:Double,N:Double):Double = 
{
    Element*N
}  
//multiplierRow function
  def multiplierRow(MatrixRow:List[Double],N:Double):List[Double] = MatrixRow match
  {
      case Nil=>Nil
      case head::rest=>List(multiplierElement(head,N)):::multiplierRow(rest,N)
  }
//multiplier function
  def multiplier(Matrix:List[List[Double]],N:Double):List[List[Double]] = Matrix match
  {
      case Nil=>Nil
      case head::rest=>List(multiplierRow(head,N)):::multiplier(rest,N)
  }
  
  
def row_sum(matrix_1_row: List[Double],matrix_2_row: List[Double]):List[Double] = matrix_1_row match
        {
           case Nil=>Nil
           case head::rest=>List(matrix_1_row.head+matrix_2_row.head):::row_sum(matrix_1_row.tail,matrix_2_row.tail)
        }
//finds dot product of 2 matrices    
   def dotSum(matrix_1:List[List[Double]],matrix_2:List[List[Double]]):List[List[Double]] = matrix_1 match
   {
        case Nil=>Nil
        case head::rest=>List(row_sum(matrix_1.head,matrix_2.head)):::dotSum(matrix_1.tail,matrix_2.tail);
   }
     
//assembly
def assembly(Image:List[List[Double]],imageSize:List[Int],w1:Double,w2:Double,b:Double,Kernel1:List[List[Double]],kernelSize1:List[Int],Kernel2:List[List[Double]],kernelSize2:List[Int],Kernel3:List[List[Double]],kernelSize3:List[Int],Size:Int):List[List[Int]] = 
{
    val temp1 = mixedLayer(Image,Kernel1,imageSize,kernelSize1,ReLu,avgPooling,Size)
    //println(temp1)
    val temp2 = mixedLayer(Image,Kernel2,imageSize,kernelSize2,ReLu,avgPooling,Size)
    //println(temp2)
    val temp3 = activationLayer((a:Double)=>a+b,dotSum(multiplier(temp1,w1),multiplier(temp2,w2)))
    //println(temp3)
    val temp3size:List[Int] = List(temp3.size,temp3.head.size)
    val temp4 = mixedLayer(temp3,Kernel3,temp3size,kernelSize3,LeakyReLu,maxPooling,Size)
    //println(temp4)
    val out = normalise(temp4)
    return out
}
}
