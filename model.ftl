<!DOCTYPE html>
<html>
<head>
<title>${title}</title>
<link rel="stylesheet" type="text/css" href="../style.css">
<meta name="AUTHOR" content="${credits}"></meta>
</head>
<body>
<center>
<h1>${title}</h1>
<applet code="org.nlogo.lite.Applet"
        width="${width}" height="${height}"
        archive="../NetLogoLite.jar">
  <param name="DefaultModel"
         value="${modelName}.nlogo">
</applet>
</center>
${documentation}
</body>
</html>
