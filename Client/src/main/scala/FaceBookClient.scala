import java.io.FileOutputStream
import java.net.URL
import java.nio.file.{Paths, Files}

import akka.actor.{ActorRef, Actor, Props, ActorSystem}
import akka.util.Timeout
import org.apache.commons.codec.binary.Base64
import spray.client.pipelining._
import spray.http.{HttpResponse, HttpRequest}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.concurrent.duration._
/**
 * Created by amarnitdgp on 11/26/2015.
 */


case class InitClientSystem(numOfUsers: Int)
case class RegisterNewUser(uid : Int)
case class UpdateFriendListOnBoot()
case class AddFriend(myUserId: Int , friendUserId: Int)
case class DeleteFriend(myUserId: Int , friendUserId: Int)
case class DeleteFriendsOps(myUserId: Int)
case class CreatePagesForUsers()
case class CreatePage(myUserId : Int, pageData : PageData)
case class DeletePage(userId : Int, pageId : Int)
case class UpdatePage(userId : Int,pageId: Int,pageData:PageData)
case class AddNewPageToUser(userId : Int)
case class GeneratePageDataandUpdatePage(userId : Int)
case class ReadPageData(userId : Int, pageId:Int)
case class CreateUserProfile(userId : Int , userName : String)
case class UpdateUserProfile(userId : Int)
case class ViewFriendsWallPosts(myUserId: Int)
case class PostOnFriendsWall(myUserId: Int , friendId :Int = -1)
case class SimulateUserInteraction(numOfUsers: Int)
case class ViewMyProfilePicture(userId : Int)
case class DeleteProfilePicture(userId : Int)
case class CreateAlbum(userId : Int)
case class ViewAlbum(userId : Int)
case class DeleteAlbum(userId : Int )


class FaceBookUser(clientSimulatorBoss: ActorRef, totalUsers: Int) extends Actor
{
  var pipeline = sendReceive
  var r = new Random
  import User._
  implicit val timeout = Timeout(3000 seconds)
  def receive = {


    case RegisterNewUser(uid : Int) =>
    {
      val prefix: String = "face"
      val userName: String =  userNameGenerator(8)
      val userId = uid//(truncateAfter(truncateBefore(self.toString(),"user/"),"#")).toInt
      var r = new Random
      var numFriends = r.nextInt(totalUsers/2 + 1)
      var myFriends = List[Int]()
      var profileData = createUserProfile(userId,userName)
      var newUser = User(userID = userId,userName =userName , friendList = myFriends, pageList = List(profileData))

      userSignUp(newUser)

    }

    case UpdateFriendListOnBoot() => {

      var numFriends = r.nextInt(totalUsers/2 + 1)
      var myFriends = List[Int]()

      for(i <- 0 to totalUsers-1) {
        if(i < totalUsers-11) {
          addFriendUtil(i + 1, i + r.nextInt(10))
          addFriendUtil(i + 1, i + r.nextInt(10))
        }
        else {
          addFriendUtil(i + 1, i - r.nextInt(10))
          addFriendUtil(i + 1, i - r.nextInt(10))
        }

      }

      Thread.sleep(5000L)
    }

    case AddFriend(myUserId: Int , friendUserId: Int) => {
      addFriendUtil(myUserId,friendUserId)
    }

    case DeleteFriend(myUserId: Int , friendUserId: Int) => {
      deleteFriendUtil(myUserId, friendUserId)
    }

    case DeleteFriendsOps(myUserId: Int) =>{


      val friendId = getOneOfFriendId(myUserId)
      if(friendId > 0)
      {
        self!DeleteFriend(myUserId,friendId)
      }
      else{
        println("No friends for user "+myUserId)
      }
    }
    case CreatePage(myUserId : Int, pageData : PageData)=>{
      createPageUtil(myUserId,pageData)
    }

    case DeletePage(userId : Int, pageId : Int)=>{
      deletePageUtil(userId,pageId)
    }

    case ReadPageData(userId : Int, pageId:Int) => {
       readPageDataUtil(userId,pageId)
    }
    case ViewFriendsWallPosts(myUserId: Int) =>
    {
      var friendId  = getOneOfFriendId(myUserId)
      println("Will view wall friendId  "+friendId + " myUserId "+myUserId)
      if(friendId > 0) {
        viewFriendsWallPostsUtil(friendId)
      }
    }
    case PostOnFriendsWall(myUserId: Int, friendId :Int) =>
    {
        if(friendId == -1)
        {
          var friendId  = getOneOfFriendId(myUserId)
          if(friendId > 0){
            postOnFriendsWallUtil(friendId)
          }
        }
         else{
            postOnFriendsWallUtil(friendId)
         }

    }
    case AddNewPageToUser(myUserId : Int)=> {

      val bools = List(true,false)
      val categoryList = List("Public", "Private")
      val userTypeList = List("Company", "LocalBusiness", "Artist/Band", "Cause/Community")
      val aboutTypeList = List("We are a corporate", "Grocery", "Eminem", "Cancer Awareness Campaign")
      val locationList = List("Chicago", "NewYork", "New Delhi", "Sydney", "Thailand", "Buenos Aires", "Istanbul")
      var randid = r.nextInt(totalUsers - 1)
      while(randid==myUserId){
        randid = r.nextInt(totalUsers - 1)
      }

      val userType = userTypeList(r.nextInt(userTypeList.length))
      val pageData = PageData(randid, userTypeList(r.nextInt(userTypeList.length)),
                     aboutTypeList(userTypeList.indexWhere(_.contentEquals(userType))),
                     bools(r.nextInt(bools.length)), bools(r.nextInt(bools.length)), categoryList(r.nextInt(categoryList.length))
                     ,locationList(r.nextInt(locationList.length)), userPhoneGenerator(), feed = List(Feed("myfeed")))


      self ! CreatePage(myUserId,pageData)

    }

    case UpdatePage(userId : Int,pageId: Int,pageData:PageData)=> {
         updatePageUtil(userId,pageId,pageData)
    }

    case GeneratePageDataandUpdatePage(myUserId : Int)=> {

      val bools = List(true,false)
      val categoryList = List("Public", "Private")
      val userTypeList = List("Company", "LocalBusiness", "Artist/Band", "Cause/Community")
      val aboutTypeList = List("We are a corporate", "Grocery", "Eminem", "Cancer Awareness Campaign")
      val locationList = List("Chicago", "NewYork", "New Delhi", "Sydney", "Thailand", "Buenos Aires", "Istanbul")

      var pageid = getUserPageId(myUserId)  //Gives a random page from list of pages of user.

      if(pageid != -1){
        val userType = userTypeList(r.nextInt(userTypeList.length))

        val pageData = PageData(pageid, userTypeList(r.nextInt(userTypeList.length)),
          aboutTypeList(userTypeList.indexWhere(_.contentEquals(userType))),
          bools(r.nextInt(bools.length)), bools(r.nextInt(bools.length)), categoryList(r.nextInt(categoryList.length))
          ,locationList(r.nextInt(locationList.length)), userPhoneGenerator(), feed = List(Feed("myfeed")))

        self ! UpdatePage(myUserId,pageid,pageData)
      }
    }

    case UpdateUserProfile(userId : Int)=>{
      updateUserProfile(userId)
    }

    case ViewMyProfilePicture(userId : Int) =>{
      println("Will view my image userID "+userId)
      viewProfilePictureUtil(userId)
    }

    case ViewAlbum(userId : Int) => {
      viewAlbum(userId)
    }
    case CreateAlbum(userId : Int) => {
        createAlbumUtil(userId)
    }

    case DeleteProfilePicture(userId : Int) =>{
      deleteProfilePictureUtil(userId)
    }

    case DeleteAlbum(userId : Int ) =>{
      deleteAlbumUtil(userId)
    }
  }



  def viewAlbum(userId : Int): Unit = {

    println("View album for "+userId)
    var result = pipeline(Get("http://localhost:8080/user/" + userId + "/getAlbum"))
    result.foreach { response =>
      var imageString = response.entity.asString
      var data = Base64.decodeBase64(imageString)
      var stream = new FileOutputStream("temp" + userId + ".png")
      stream.write(data)
      println(s"Image "+"temp"+userId+".png"+" is stored at :"+System.getProperty("user.dir"))
    }

  }

  def deleteAlbumUtil(userId : Int ) ={
    println("Deleting album for userId "+userId)
    var result = pipeline(Delete("http://localhost:8080/user/" + userId + "/deleteAlbum"))
    result.foreach { response =>
      println(s"Delete Album with status ${response.status} and content:\n${response.entity.asString}")
    }

  }

  def createAlbumUtil(userId: Int) ={

    var mUrl = List (System.getProperty("user.dir")+"\\img13.png", System.getProperty("user.dir")+"\\img14.png", System.getProperty("user.dir")+"\\img15.png")
    var inUrl = r.nextInt(mUrl.length)

    var httpData = Files.readAllBytes(Paths.get(mUrl(inUrl)))
    val bytes64 = Base64.encodeBase64(httpData)
    val streamInString = new String(bytes64)

    var cIndex = userId
    while (cIndex == userId ) {
      cIndex = r.nextInt(1000)
    }
    var photo = ImageUploaded(imageID = cIndex, blob = streamInString)

    var result = pipeline(Put("http://localhost:8080/user/" + userId + "/addPhotoToAlbum", photo))
    result.foreach { response =>
      println(s"Create Album success status ${response.status} and content:\n${response.entity.asString}")
    }
  }

  def deleteProfilePictureUtil(userId: Int) = {

    println("Deleting Profile picture for "+userId)
    val result = pipeline(Delete("http://localhost:8080/user/" + userId + "/deleteImage/" + userId))
    result.foreach { response =>
      println(s"Request completed with status ${response.status} and content:\n${response.entity.asString}")
    }
  }

  def viewProfilePictureUtil(myUserId : Int) ={
    val timeout = 3000.seconds
    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive ~> unmarshal[HttpResponse]
    var result = pipeline(Get("http://localhost:8080/user/" + myUserId + "/getAlbum" ))
    var imageString = Await.result(result, timeout).entity.asString
    if (!imageString.equals("")){
      var data = Base64.decodeBase64(imageString)
      var stream = new FileOutputStream("temp" + myUserId + ".png")
      stream.write(data)
      println(s"Image "+"temp"+myUserId+".png"+" stored at :"+System.getProperty("user.dir"))
    }
  }

  def createProfileImage(u :User, imageId : Int): ImageUploaded ={

    var mUrl = List (System.getProperty("user.dir")+"\\img13.png", System.getProperty("user.dir")+"\\img14.png", System.getProperty("user.dir")+"\\img15.png")

    var inUrl = r.nextInt(mUrl.length)
    var httpData = Files.readAllBytes(Paths.get(mUrl(inUrl)))
    val bytes64 = Base64.encodeBase64(httpData)
    val streamInString = new String(bytes64)

    var photo = ImageUploaded(imageID = imageId, blob = streamInString)
    return photo
  }

  def userSignUp(u : User): Unit = {

    val photo = createProfileImage(u, u.userID)
    var imList = List(photo)

    var userImg : UserImage = UserImage(user= u, imageList = imList)
    val result =  pipeline(Post("http://localhost:8080/user/add", userImg))
    result.foreach { response =>
      println(s"User signup completed with status ${response.status} and content:\n${response.entity.asString}")
    }
  }

  def addFriendUtil(myUserId: Int , friendUserId: Int):Unit ={

    var result = pipeline(Put("http://localhost:8080/user/addFriend?myId=" + myUserId + "&frndUserId=" + friendUserId))
    result.foreach { response =>
      println(s"Add Friend completed with status ${response.status} and content:\n${response.entity.asString}")
    }
  }

  def deleteFriendUtil(myUserId: Int , friendUserId: Int):Unit ={
    var result = pipeline(Put("http://localhost:8080/user/deleteFriend?myId=" + myUserId + "&frndUserId=" + friendUserId))
    result.foreach { response =>
      println(s"Delete Friend Success")
    }
  }

  def createUserProfile(userId:Int, userName : String) : PageData = {

    val bools = List(true,false)
    val categoryList = List("Public", "Private")
    val userTypeList = List("Individual", "Company", "LocalBusiness", "Artist/Band", "Cause/Community")
    val aboutTypeList = List("I am individual", "We are a corporate", "Grocery", "Eminem", "Cancer Awareness Campaign")
    val locationList = List("Chicago", "NewYork", "New Delhi", "Sydney", "Thailand", "Buenos Aires", "Istanbul")

    var pageId = userId
    val userType = userTypeList(r.nextInt(userTypeList.length))

    val pageData = PageData(pageId, userTypeList(r.nextInt(userTypeList.length)),
      aboutTypeList(userTypeList.indexWhere(_.contentEquals(userType))),
      bools(r.nextInt(bools.length)), bools(r.nextInt(bools.length)), categoryList(r.nextInt(categoryList.length))
      ,locationList(r.nextInt(locationList.length)), userPhoneGenerator(), feed = List(Feed("Welcome "+userName+" to FaceBook!!")))

    pageData
  }

  def updateUserProfile(userId : Int) : Unit ={

    val bools = List(true,false)
    val categoryList = List("Public", "Private")
    val userTypeList = List("Individual", "Company", "LocalBusiness", "Artist/Band", "Cause/Community")
    val aboutTypeList = List("I am individual", "We are a corporate", "Grocery", "Eminem", "Cancer Awareness Campaign")
    val locationList = List("Chicago", "NewYork", "New Delhi", "Sydney", "Thailand", "Buenos Aires", "Istanbul")

    var pageId = userId
    val userType = userTypeList(r.nextInt(userTypeList.length))

    val pageData = PageData(pageId, userTypeList(r.nextInt(userTypeList.length)),
      aboutTypeList(userTypeList.indexWhere(_.contentEquals(userType))),
      bools(r.nextInt(bools.length)), bools(r.nextInt(bools.length)), categoryList(r.nextInt(categoryList.length))
      ,locationList(r.nextInt(locationList.length)), userPhoneGenerator(), feed = List(Feed("Its a sunny day today!!")))

    updateUserProfileUtil(userId,pageData)

  }
  def updateUserProfileUtil(userId: Int , pageData: PageData):Unit ={

    /* Hack added as was unable to pass PageData directly hence passing page data as User induced value.
       Error:(115, 30) could not find implicit value for evidence parameter of type spray.httpx.marshalling.Marshaller[PageData]
       var result = pipeline(Put("http://localhost:8080/user/createPage",pageData))*/

    var newUser =  User(userId, "dummy", friendList = List[Int](), pageList = List(pageData))
    var result = pipeline(Put("http://localhost:8080/user/updateProfile?userId="+userId+"&pageId="+userId,newUser))
    result.foreach { response =>
      println(s"profile page updated with status ${response.status} and content ${response.entity.asString}")
    }
  }

  def createPageUtil(userId: Int , pageData: PageData):Unit ={

  /* Hack added as was unable to pass PageData directly hence passing page data as User induced value.
     Error:(115, 30) could not find implicit value for evidence parameter of type spray.httpx.marshalling.Marshaller[PageData]
     var result = pipeline(Put("http://localhost:8080/user/createPage",pageData))*/

    var newUser =  User(userId, "dummy", friendList = List[Int](), pageList = List(pageData))
    println("creating page for :"+userId+" Page Data "+pageData)
    var result = pipeline(Put("http://localhost:8080/user/createPage?userId="+userId,newUser))
    result.foreach { response =>
      println(s"Page created with status ${response.status} and content ${response.entity.asString}")
    }
  }

  def deletePageUtil(userId: Int , pageId :Int):Unit ={

    println("deleting page for :"+userId+"pageId"+pageId)
    var result = pipeline(Put("http://localhost:8080/user/deletePage?userId="+userId+"&pageId="+pageId))
    result.foreach { response =>
      println(s"Delete Page with status ${response.status} and content ${response.entity.asString}")
    }
  }

  def updatePageUtil(userId: Int , pageId: Int, pageData: PageData):Unit ={

    /* Hack added as was unable to pass PageData directly hence passing page data as User induced value.
       Error:(115, 30) could not find implicit value for evidence parameter of type spray.httpx.marshalling.Marshaller[PageData]
       var result = pipeline(Put("http://localhost:8080/user/createPage",pageData))*/

    var newUser =  User(userId, "dummy", friendList = List[Int](), pageList = List(pageData))
    var result = pipeline(Put("http://localhost:8080/user/updatePage?userId="+userId+"&pageId"+pageId,newUser))
    result.foreach { response =>
      println(s"Update page with status ${response.status} and content ${response.entity.asString}")
    }
  }

  def readPageDataUtil(userId: Int,pageId: Int): Unit = {

    var result = pipeline(Get("http://localhost:8080/user/readUserPage?userId="+userId+"&pageId="+pageId))
    result.foreach { response =>
      println(s"Read page with status ${response.status} and content ${response.entity.asString}")
    }
  }

  def getUserPageId(userId:Int): Int = {

    var URL = "http://localhost:8080/user/getAnyPageId?userId="+userId
    implicit val timeout = 3000.seconds
    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive ~> unmarshal[HttpResponse]

    val f: Future[HttpResponse] = pipeline(Get(URL))
    val page = Await.result(f, timeout)
    var sr = page.entity.asString

    try {
      val pageId = sr.toInt
      return pageId
    }
    catch{
      case ex : NumberFormatException => {
        println(ex.getMessage)
        return -1
      }
    }
  }

  def postOnFriendsWallUtil(friendId : Int): Unit = {


    val samplefeedList = List("Hi !! How have you been ?", "Happy Birthday !!!", "Happy Halloween !!!")
    var feed = Feed(samplefeedList(r.nextInt(samplefeedList.length)))
    var result = pipeline(Post("http://localhost:8080/user/postOnWall?friendId="+friendId ,feed))
    result.foreach { response =>
      println(s"Posting on friends wall status ${response.status} and content ${response.entity.asString}")
    }

  }

  def getOneOfFriendId(userId:Int): Int = {
    var URL = "http://localhost:8080/user/getMyFriendId?userId="+userId
    implicit val timeout = 3000.seconds
    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive ~> unmarshal[HttpResponse]

    if(userId>0) {
      val f: Future[HttpResponse] = pipeline(Get(URL))
      val res = Await.result(f, timeout)
      var str = res.entity.asString

      try {
        val friendId = str.toInt
        return friendId
      }
      catch {
        case ex: NumberFormatException => {
          println(ex.getMessage)
          return -1
        }
      }
    }
    else {
      return -1
    }
  }

  def viewFriendsWallPostsUtil(friendUserId: Int): Unit = {

    var result = pipeline(Get("http://localhost:8080/user/viewFriendWallPosts?friendUserId="+friendUserId))
    result.foreach { response =>
      println(s"Friends wall post with status ${response.status} and content ${response.entity.asString}")
    }
  }

  def userNameGenerator( length: Int ) : String = {
    val  AB = "abcdefghijklmnopqrstuvwxyz"
    val r = new scala.util.Random
    val sb = new StringBuilder
    for(i <- 1 to length)
    sb.append(AB.charAt(r.nextInt(AB.length())))
    sb.toString()
  }

  def userPhoneGenerator(length: Int = 10 ) : String = {
    val  num = "123456789"
    val r = new scala.util.Random
    val sb = new StringBuilder
    for(i <- 1 to length)
      sb.append(num.charAt(r.nextInt(num.length())))
    sb.toString()
  }

  def truncateBefore(s: String, p: String) = {
    s.substring(s.indexOf(p) + p.length, s.length)
  }

  def truncateAfter(s: String, p: String) = {
    s.substring(0, s.indexOf(p) + p.length-1)
  }


}
class ClientUnitOperation extends Actor {
  var numOfFacebookUsers: Int = 0
  var actors: Array[ActorRef] = null
  var pipeline = sendReceive
  implicit val timeout = Timeout(3000 seconds)
  def receive = {
    case InitClientSystem(numOfUsers: Int) => {

      numOfFacebookUsers = numOfUsers
      actors = new Array[ActorRef](numOfFacebookUsers)

      /*Create FaceBook Users*/
      for (i <- 0 to numOfFacebookUsers - 1) {
        actors(i) = context.system.actorOf(Props(new FaceBookUser(self,numOfFacebookUsers)), (i+1).toString)
      }

      /*Register each user on FaceBook domain*/
      for (i <- 0 to numOfFacebookUsers - 1) {
        actors(i) ! RegisterNewUser(i+1)
      }

      /*Update friend list for each user*/
      actors(0) ! UpdateFriendListOnBoot()

    }

    case SimulateUserInteraction(numOfUsers: Int) => {


      /*Simulation Distribution*/

      /*A sets of users Update their Profiles */
      for (i <- 0 to numOfFacebookUsers*15/100) {
        actors(i)!UpdateUserProfile(i+1)
      }

      /*Randomly update Pages associated with a user*/
      for (i <- 0 to numOfFacebookUsers*27/100) {
        actors(i)!GeneratePageDataandUpdatePage(i+1)
      }

      /*Users see their friends wall*/
      for (i <- 0 to numOfFacebookUsers*44/100) {
        actors(i)!ViewFriendsWallPosts(i+1)
      }

     /*Users post on their friends wall*/
      for (i <- 0 to numOfFacebookUsers*22/100){
        actors(i)!PostOnFriendsWall(i+1)
      }

      /*Deleting friends from x` list*/
      for (i <- 0 to numOfFacebookUsers*1/100) {
        actors(i)!DeleteFriendsOps(i+1)
      }


      /*View Profile Picture  */
      for (i <- 0 to  numOfFacebookUsers*25/100) {
        actors(i)!ViewMyProfilePicture(i+1)
      }

      /*Create Album  */
      for (i <- 0 to numOfFacebookUsers*25/100) {
        actors(i)!CreateAlbum(i+1)
      }

      /*View Album  */
      for (i <- 0 to (numOfFacebookUsers)*15/100) {
        actors(i)!ViewAlbum(i+1)
      }

      /*Delete Profile Picture */
      for (i <- 0 to (numOfFacebookUsers)*15/100) {
        actors(i)!DeleteProfilePicture(i+1)
      }

      /*Delete Album */
      for (i <- 0 to (numOfFacebookUsers)*15/100) {
        actors(i)!DeleteAlbum(i+1)

      }

    }

    case CreatePagesForUsers() => {

      val userTypeList = List("Company","LocalBusiness","Artist/Band","Cause/Community")
      val aboutTypeList = List("We are a corporate" ,"Grocery","Eminem","Cancer Awareness Campaign")
      val categoryList = List("Public","Private")
      val locationList = List ("Chicago","NewYork","New Delhi","Sydney","Thailand","Buenos Aires","Istanbul")
      var pageidList = List[Int]()

      var r = new Random
      var randid = r.nextInt(numOfFacebookUsers-1)
      for (i <- 0 to numOfFacebookUsers - 1) {
        while (pageidList.contains(randid)){
          randid=r.nextInt(numOfFacebookUsers-1)
        }
        pageidList = randid :: pageidList

        val pageid  = randid
        val userType = userTypeList(r.nextInt(userTypeList.length))
        val about = aboutTypeList(userTypeList.indexWhere(_.contentEquals(userType)))
        val cancheckin = true
        val canpost = true
        val catgry = categoryList(r.nextInt(categoryList.length))
        val loc = locationList(r.nextInt(locationList.length))
        val phone = userPhoneGenerator()
        var fd = List(Feed("myfeed"))
        val pageData = PageData(pageid,userType,about,cancheckin,canpost,catgry,loc,phone,feed=fd)

        //actors(i)!CreatePage(i+1,pageData)
      }

      Thread.sleep(5000L)
    }
  }


  def getUsernameFromUserId(userId : Int): Unit =
  {
    val result =  pipeline(Get("http://localhost:8080/user/fetchName?userId="+userId))
     result.foreach { response =>
       val uname = response.entity.asString
        println(s"Request completed with status ${response.status} and content username: "+uname)
    }
  }

  def userPhoneGenerator(length: Int = 10 ) : String = {
    val  num = "123456789"
    val r = new scala.util.Random
    val sb = new StringBuilder
    for(i <- 1 to length)
      sb.append(num.charAt(r.nextInt(num.length())))
      sb.toString()
  }

}

object FaceBookClientSimulator extends App{

  var numOfUsers = 2000
  implicit val timeout = Timeout(3000 seconds)

  /*Read number of facebook users to be registered for simulation*/
  if (args.length != 1) {
    println("Default "+numOfUsers+" FaceBook users is being created !!!")
  }
  else{
    numOfUsers = args(0).toInt
    println(numOfUsers+" FaceBook users is being created !!!")
  }
    /*Create Simulator System and Simulator Actor Boss*/

    val System = ActorSystem("ClientSimulatorSystem")
    val clientSimulator = System.actorOf(Props[ClientUnitOperation],name="ClientSimulatorBoss")

    /*Init FBUser or Client System*/
    println("System Bootup .....")
    clientSimulator!InitClientSystem(numOfUsers)

    println("Initiating User Simulation now ...")
    println("Few moments to set up simulation  ...")
    /*Let Boot Complete then intiate User Simulation*/
    System.scheduler.schedule(20 seconds, 5000 seconds) {
      clientSimulator!SimulateUserInteraction(numOfUsers)
    }
}
