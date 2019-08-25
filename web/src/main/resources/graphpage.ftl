<html>
<title>HTTP Server in Java</title>
<body><!DOCTYPE html>
<head>
    <meta charset="UTF-8">
    <title>${pageTitle}</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <script type="text/x-mathjax-config">
  MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});
</script>
<script type="text/javascript"
  src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>
</head>
<body>
	<#list figures as fig>
		<h3>${fig.title}</h3>
		<p>${fig.text}</p>
		<div id='${fig.id}' ></div>
		${fig.figure}
	</#list>
 </body>
</html>