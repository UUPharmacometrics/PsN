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
    <div id="footer" style="padding-top:2px;padding-right:2px;">
       <a href="http://www.sourceforge.net/" target="_blank" ><img src="graphics/sflogo.png" alt="SourceForge" border="0"></a>
    </div>
  </div>
</body>
</html>
