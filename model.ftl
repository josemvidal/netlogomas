<!DOCTYPE html>
<html>
<head>
<title>${title}</title>
<link rel="stylesheet" type="text/css" href="../style.css">
<meta name="AUTHOR" content="${credits}"></meta>
</head>
<body>
<center>
<applet code="org.nlogo.lite.Applet"
        width="${width}" height="${height}"
        archive="../NetLogoLite.jar,table/table.jar">
  <param name="DefaultModel"
         value="${modelName}.nlogo">
</applet>
</center>
<a id="download" href="${modelName}.nlogo">Download</a>
${documentation}
</body>
</html>
