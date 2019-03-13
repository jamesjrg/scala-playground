/*
Views:
*/

//fail
(1 to 1000000000).filter(_ % 2 == 0).take(10).toList

//success
(1 to 1000000000).view.filter(_ % 2 == 0).take(10).toList

//success
(1 to 1000000000).view.take(1000000000).filter(_ % 2 == 0).sum