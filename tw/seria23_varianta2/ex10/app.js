const express=require('express');
app=express();
app.use('/ex10.html',express.urlencoded({extended:false}));

orase=[
	{
		nume:"Aa",
		populatie:14000,
		capitala:true
	},
	{
		nume:"Bb",
		populatie:7000,
		capitala:false
	},
	{
		nume:"Cc",
		populatie:19000,
		capitala:false
	},
	{
		nume:"Dd",
		populatie:5000,
		capitala:false
	},
	{
		nume:"Ee",
		populatie:8000,
		capitala:true
	},
	{
		nume:"Ff",
		populatie:12000,
		capitala:false
	},
	{
		nume:"Gg",
		populatie:20000,
		capitala:true
	}
]
app.get("/ex10.html", function(req,res){

	res.sendFile(__dirname+"/ex10.html");
});

//Completati
app.post("/ex10.html", function(req, res) {
	const rezultat = orase.filter(function(oras) {
		if (req.body.capitala === "da") {
			if (!oras.capitala) {
				return false;
			}
		} else if (req.body.capitala === "nu") {
			if (oras.capitala) {
				return false;
			}
		}

		if (req.body.tip_oras === "mic") {
			return oras.populatie < 10000;
		} else {
			return oras.populatie >= 10000;
		}
	});

	res.write(" \
	<!DOCTYPE html> \
	<html> \
		<head> \
			<meta charset='UTF-8'/> \
			<title>Rezultat</title> \
		</head> \
		<body> \
			<table> \
				<thead> \
					<td>Nume</td> \
					<td>Populație</td> \
					<td>Capitală</td> \
				</thead>\
	");

	for (let oras of rezultat) {
		res.write("<tr><td>" + oras.nume + "</td><td>" + oras.populatie + "</td><td>" + oras.capitala + "</td></tr>");
	}

	res.end(" \
			</table> \
		</body> \
	</html> \
	");
});


app.listen(8010);
console.log("Serverul a pornit");
