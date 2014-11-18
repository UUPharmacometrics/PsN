<!DOCTYPE html PUBLIC “-//W3C//DTD XHTML 1.0 Strict//EN”   “http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd”>
<html>
<head>
  <title>PsN :: Home</title>
  <link rel="stylesheet" type="text/css" href="style/style.css">
  <script type="text/javascript" src="javascript/jquery.js"></script>
</head>
<body >
  <div id="wrap">
    <div id="header"></div>
    <div id="nav"><?php include "menu.php"; ?></div>
    <div id="main"><?php
      if(isset($_GET['page']) )
      {
        include $_GET['page'];
      }
      else{
        include "home.php";
      }
    ?></div>
    <div id="footer" style="padding-top:2px;padding-right:2px;padding-left:2px;">
        <a href="http://www.ddmore.eu" target="_blank"><img src="http://www.ddmore.eu/sites/ddmore/themes/ddmore/logo.png" border="0" width="150" style="background-color:white;" /></a>
        <a href="http://www.ideal.rwth-aachen.de" target="_blank"><img src="http://www.ideal.rwth-aachen.de/wp-content/uploads/2013/08/banner1.png" border="0" width="105" style="background-color:white;" /></a>
		<a href="http://sourceforge.net/projects/psn" target="_blank"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=212691&type=13" border="0" style="float:right;" /></a>
    </div>
  </div>
</body>
</html>
