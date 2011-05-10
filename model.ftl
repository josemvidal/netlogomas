<!DOCTYPE html>
<html>
<head>
<title>${title}</title>
<link rel="stylesheet" type="text/css" href="../style.css">
<meta name="AUTHOR" content="Jose M Vidal"></meta>
<script type="text/javascript">
var _gaq = _gaq || [];
 _gaq.push(['_setAccount', 'UA-279322-1']);
 _gaq.push(['_trackPageview']);
(function() {
   var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
   ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
   var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
 })();
</script>
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
