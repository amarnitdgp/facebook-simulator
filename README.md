# facebook-simulator
UFL Distributed OS Project - Scala/Akka/Spray

This facebook simulator aims at simulating facebook like protocol using REST framework, Spray Can and Scala Akka actor model.

Facebook User Simulation Distribution
Based on the number of users we will be deciding the Simulation distribution of Facebook operations such as Create User, Create Profile, Add Friend, Remove Friend, Post on Friends Wall, Update Profile, Create Album etc.

1) Per the research statistics - 44% of Facebook users “like” content posted by their friends which is synonymous to at least first reading the post hence post reads are simulated at 44% of total users.
- Read Friends Post - 44 %
2) Per the research statistics - 15% of Facebook users update their own status
- Update user profile - 15 %
3) Per the research statistics - 22% comment on another’s post or status is synonymous to posting on friends wall
- Post on friends wall - 22 %
4) Per the research statistics - On an average 27% of user create new Pages for a cause or community
- New Page Creation associated with a user for a cause - 27 %
5) Per the research statistics – Approximately 1 % of the users unfriend a friends
- Remove a friend from friend list = 1%
6) Profile Image Deletion / AlbumCreation / View Album /Album Deletion are distributed based assumption 15%
- Picture Operations = 15%

User Study References
https://zephoria.com/top-15-valuable-facebook-statistics/ below is the distribution of operations
http://www.pewresearch.org/fact-tank/2014/02/03/6-new-facts-about-facebook/
http://www.pewinternet.org/2011/06/16/part-2-who-are-social-networking-site-users/?beta=true&utm_expid=53098246-2.Lly4CFSVQG2lphsg-KopIg.1&utm_referrer=https%3A%2F%2Fwww.google.com%2F
Other Project references:
https://www.youtube.com/watch?v=XPuOlpWEvmw
http://stackoverflow.com/questions/9714831/how-can-i-have-an-akka-actor-executed-every-5-min
http://spray.io/documentation/1.2.2/spray-routing/method-directives/put/
http://spray.io/documentation/1.1-SNAPSHOT/api/index.html#spray.http.HttpResponse
http://spray.io/documentation/1.2.2/spray-client/

