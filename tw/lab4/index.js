const container = document.getElementById("figure-container");

const sources = [
    "https://images.pexels.com/photos/356378/pexels-photo-356378.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=750&w=1260",
    "https://upload.wikimedia.org/wikipedia/commons/3/31/Ice_Cream_dessert_02.jpg",
    "https://cdn.britannica.com/77/170477-050-1C747EE3/Laptop-computer.jpg",
    "https://thumbor.forbes.com/thumbor/960x0/https%3A%2F%2Fblogs-images.forbes.com%2Ferikkain%2Ffiles%2F2018%2F10%2Fballoons-2.jpg"
];

const descriptions = [
    "Dog lying on the grass",
    "Ice cream dessert",
    "A modern laptop",
    "Bunches of baloons"
];

let currentDir = 0;

function insertFigure() {
    const index = Math.floor(Math.random() * sources.length);

    const src = sources[index];
    const caption = descriptions[index];

    const figureElem = document.createElement("figure");
    figureElem.onclick = figureElem.remove.bind(figureElem);

    const imageElem = document.createElement("img");
    imageElem.src = src;
    imageElem.alt = caption;

    const figcaptionElem = document.createElement("figcaption");
    figcaptionElem.textContent = caption;

    figureElem.appendChild(imageElem);
    figureElem.appendChild(figcaptionElem);

    container.appendChild(figureElem);
}

function changeLayout() {
    if (currentDir == 0) {
        container.style.flexDirection = "column";
        currentDir = 1;
    } else {
        container.style.flexDirection = "row";
        currentDir = 0;
    }
}
