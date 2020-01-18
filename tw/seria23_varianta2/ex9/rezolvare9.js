function randomInt(a, b) {
    return a + Math.random() * (b - a);
}

function randomColor() {
    const r = randomInt(0, 255);
    const g = randomInt(0, 255);
    const b = randomInt(0, 255);

    return "rgb(" + r + ", " + g + ", " + b +")";
}

window.addEventListener("load", function() {
    const button = document.getElementById("btn");

    const savedColor = localStorage.getItem("color");
    if (savedColor !== null) {
        button.style.backgroundColor = savedColor;
    }

    button.addEventListener("click", function() {
        const color = randomColor();
        button.style.backgroundColor = color;
        localStorage.setItem("color", color);
    });
});
