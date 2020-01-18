const colorsVector = [
    "red",
    "blue",
    "green",
    "yellow",
    "purple"
];

function createSquare(i) {
    const square = document.createElement("div");

    square.className = "patrat";

    square.style.width = (i * 10 + 20) + "px";
    square.style.height = (i * 10 + 20) + "px";

    square.addEventListener("click", function(event) {
        square.style.backgroundColor = colorsVector[i];

        event.stopPropagation();
    });

    return square;
}

window.addEventListener("load", function() {
    let prev = null;
    for (let i = 4; i >= 0; --i) {
        const square = createSquare(i);

        if (prev !== null) {
            prev.appendChild(square);
        } else {
            document.body.appendChild(square);
        }

        prev = square;
    }
});
