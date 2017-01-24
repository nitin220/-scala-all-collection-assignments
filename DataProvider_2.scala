//package com.knoldus.collectionAssignments


//import com.knoldus.ManipulationOnStudentMarks._

case class Student(id: Long, name: String,gen:gender.Value)

case class Marks(subjectId: Int,studentId: Long, marksObtained: Float)
case class ScoreCard (studentId: Long, marks: Map[Long, Float], percentage: Float)
object gender extends Enumeration
{
	val male=Value{"male"}
	val female=Value{"female"}
}

class ManipulationGender(sourceStudent:List[Student],sourceMarks:List[List[List[Marks]]])
{
  
	def getScorecardByGender(source:List[Map[String,ScoreCard]])=
	{
		val (male,female)=sourceStudent partition(x=>x.gen==gender.male)
		val scoreList=source.map(_.map(x=>x._2))
		val onlyScorelist=scoreList.flatMap(_.map(x=>x))
		//println(onlyScorelist)
		val f=for{
				check<-0 until male.size
			}yield if(male(check).id==onlyScorelist(check).studentId) onlyScorelist(check)
		println(f) 
	}
	def search(name:String,map:Map[String,ScoreCard])=
    
	{
      
		val result=for{x<-map if(x._1.equalsIgnoreCase(name))} yield  x._2
    
		result.toList	   
	}

   
	
	def search_score(name:String,source:List[Map[String,ScoreCard]])=
	{
		val forbeing=for
			{
        
				check<-0 until  source.size
      
			} yield search(name,source(check))
		val score=forbeing.toList.filter{_!=Nil} 
		if(score.size!=0)
		{
			println("Name is "+name)
			score flatMap(_ map(x=>println("Student Id="+x.studentId+" Total Marks="+x.marks(x.studentId)+" Precentage="+x.percentage)))
			
		}
	}
	def generateScoreCard(source:List[Map[String,ScoreCard]])=
	{
		val name=sourceStudent map(x=>x.name)
		val name_set=name.toSet
		val nameListWithNoDuplicate=name_set.toList.reverse
		val result=for
			{
        
				check<-0 until  nameListWithNoDuplicate.size
      
			} yield search_score(nameListWithNoDuplicate(check),source)
	}
	def generateMap():List[Map[String,ScoreCard]]=
    
	{
      
		val id=sourceStudent map(x=>x.id)
      
		val map=id map(x=>marks(x))
      
		val nam=sourceStudent map(x=>x.name)
      
		val percent=map map(x=>get_p(x))
      
		val result=for
                  
			{
                    
				check<- 0 until id.size
                 
			}yield Map(nam(check)->new ScoreCard(id(check),map(check),percent(check)))
     
		result.toList
    
	}
    
	def getScorecardByName(name:String,element:List[Map[String,ScoreCard]]):String= 
	{
       
		val forbeing=for
			{
        
				check<-0 until  element.size
      
			} yield search(name,element(check))
      
		val forbeing_list=forbeing.toList
		val score=forbeing_list.filter{_!=Nil} 
		if(score.size!=0)
		{
			println("\nName is "+name)
			score flatMap(_ map(x=>println("Student Id="+x.studentId+" Total Marks="+x.marks(x.studentId)+" Precentage="+x.percentage)))
			"Data Found!"
		}
		else "No Data Found!"
	}
     	
	def get_p(map:Map[Long, Float]):Float=
    
	{
      
		val list_map=map.toList
      
		val (id,mark)=list_map.unzip
      
		val res=mark(0).toFloat
      
		res/5

    
	}
  
	def marks(id: Long): Map[Long, Float] = 
	{
    
		id match	 
		{
      
			case 1 =>
        
				val mark=getMarks(id)
        
				Map(id->mark)

        
			case 2=>
          		
				val mark=getMarks(id)
          
				Map(id->mark)
        
			case 3=>
          
				val mark=getMarks(id)
          
				Map(id->mark)
        	
			case 4=>
          
				val mark=getMarks(id)
          
				Map(id->mark)
        
			case 5=>
          
				val mark=getMarks(id)
          
				Map(id->mark)
        
			case 6=>
          
				val mark=getMarks(id)
          
				Map(id->mark)
        		
			case 7=>
          
				val mark=getMarks(id)
          
				Map(id->mark)
        
			case 8=>
          	
				val mark=getMarks(id)
          
				Map(id->mark)
        
			case 9=>
          			
				val mark=getMarks(id)
          
				Map(id->mark)
        
			case 10=>
          
				val mark=getMarks(id)
          
				Map(id->mark)


    		
		}
  
	}

  
	def getMarks(id: Long): Float = 
	{
    
		val fetchList = sourceMarks flatMap (_ flatMap (_ map (x => x)))
    
		val marksList = fetchList groupBy (_.studentId == id)
    
		val actualList = marksList(true)
    
		val marks = (0 until actualList.size).map { case check => actualList(check).marksObtained}
    
		val result = marks.toList.foldLeft(0.0) { (x, y) => x + y }
    
		result.toFloat

  
	}

}

  

object DataProvider_2 
{

    
	def main(args: Array[String]) 
	{

      
		val student_1 = Student(1, "Nitin Aggarwal",gender.male)

      
		val student_2 = Student(2, "Vandana Yadav",gender.female)

      
		val student_3 = Student(3, "Isha Jain",gender.female)

      
		val student_4 = Student(4, "Kavit Pandey",gender.male)

      
		val student_5 = Student(5, "Goldy Gupta",gender.male)

      
		val student_6 = Student(6, "Manoj Tomar",gender.male)

      
		val student_7 = Student(7, "Nitin Aggarwal",gender.male)

      
		val student_8 = Student(8, "Avreen Kaur",gender.female)

      
		val student_9 = Student(9, "Pooja Saini",gender.female)

      
		val student_10 = Student(10, "Tarun Sinha",gender.male)



      

		val maths_1 = Marks(1, 1, 86)

      
		val maths_2 = Marks(1, 2, 85)

      
		val maths_3 = Marks(1, 3, 96)

      
		val maths_4 = Marks(1, 4, 76)

      
		val maths_5 = Marks(1, 5, 70)

      
		val maths_6 = Marks(1, 6, 69)

      
		val maths_7 = Marks(1, 7, 30)

      
		val maths_8 = Marks(1, 8, 38)

      
		val maths_9 = Marks(1, 9, 28)

      
		val maths_10 = Marks(1, 10, 39)



      
	
		val science_1 = Marks(2, 1, 88)

		val science_2 = Marks(2, 2, 83)

      
		val science_3 = Marks(2, 3, 95)

      
		val science_4 = Marks(2, 4, 70)

      
		val science_5 = Marks(2, 5, 65)

      
		val science_6 = Marks(2, 6, 67)

      
		val science_7 = Marks(2, 7, 37)

      
		val science_8 = Marks(2, 8, 55)

      
		val science_9 = Marks(2, 9, 34)

      
		val science_10 = Marks(2, 10, 25)



      

		val hindi_1 = Marks(3, 1, 85)

      
		val hindi_2 = Marks(3, 2, 86)

      
		val hindi_3 = Marks(3, 3, 94)

      
		val hindi_4 = Marks(3, 4, 73)

      
		val hindi_5 = Marks(3, 5, 68)

      
		val hindi_6 = Marks(3, 6, 62)

      
		val hindi_7 = Marks(3, 7, 40)

      
		val hindi_8 = Marks(3, 8, 35)

      
		val hindi_9 = Marks(3, 9, 44)

      
		val hindi_10 = Marks(3, 10, 30)



      

		val Eng_1 = Marks(4, 1, 86)

      
		val Eng_2 = Marks(4, 2, 85)

      
		val Eng_3 = Marks(4, 3, 97)

     
		val Eng_4 = Marks(4, 4, 76)

      
		val Eng_5 = Marks(4, 5, 70)

      
		val Eng_6 = Marks(4, 6, 69)

      
		val Eng_7 = Marks(4, 7, 30)

      
		val Eng_8 = Marks(4, 8, 38)

      
		val Eng_9 = Marks(4, 9, 28)

      
		val Eng_10 = Marks(4, 10, 39)



      

		val comp_1 = Marks(5, 1, 88)

      
		val comp_2 = Marks(5, 2, 83)

      
		val comp_3 = Marks(5, 3, 99)

      
		val comp_4 = Marks(5, 4, 70)

      
		val comp_5 = Marks(5, 5, 65)

      
		val comp_6 = Marks(5, 6, 67)

      
		val comp_7 = Marks(5, 7, 37)

      
		val comp_8 = Marks(5, 8, 55)

      
		val comp_9 = Marks(5, 9, 34)

      
		val comp_10 = Marks(5, 10, 25)



      
		val student_list = List(student_1, student_2, student_3, student_4, student_5, student_6, student_7, student_8, student_9, student_10)

      val Marks_stud_1 = List(maths_1, science_1, hindi_1, Eng_1, comp_1)

      

		val Marks_stud_2 = List(maths_2, science_2, hindi_2, Eng_2, comp_2)

      
		val Marks_stud_3 = List(maths_3, science_3, hindi_3, Eng_3, comp_3)

      
		val Marks_stud_4 = List(maths_4, science_4, hindi_4, Eng_4, comp_4)

      
		val Marks_stud_5 = List(maths_5, science_5, hindi_5, Eng_5, comp_5)

      
		val Marks_stud_6 = List(maths_6, science_6, hindi_6, Eng_6, comp_6)

      
		val Marks_stud_7 = List(maths_7, science_7, hindi_7, Eng_7, comp_7)

      
		val Marks_stud_8 = List(maths_8, science_8, hindi_8, Eng_8, comp_8)

      
		val Marks_stud_9 = List(maths_9, science_9, hindi_9, Eng_9, comp_9)

      
		val Marks_stud_10 = List(maths_10, science_10, hindi_10, Eng_10, comp_10)



      

		val marks_list = List(List(Marks_stud_1), List(Marks_stud_2), List(Marks_stud_3), List(Marks_stud_4), List(Marks_stud_5), List(Marks_stud_6), List(Marks_stud_7), List(Marks_stud_8), List(Marks_stud_9), List(Marks_stud_10))

      
		val obj = new ManipulationGender(student_list, marks_list)
      
		val list_map=obj.generateMap()

		//println(list_map)
		//println(student_list)
		obj.getScorecardByGender(list_map) 
	}
  
}



