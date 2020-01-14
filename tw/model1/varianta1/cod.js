let parent = null;
let doubleClickLocationDisplay = null;
let cells = [];

window.addEventListener("load", function() {
  parent = document.createElement("div");
  parent.id = "parinte";

  parent.appendChild(createInputGroup());

  parent.addEventListener("click", function onClickParent() {
    // Nu adaug celulele de mai multe ori
    parent.removeEventListener("click", onClickParent);

    setTimeout(createCells, 5000);
  });

  document.body.appendChild(parent);

  doubleClickLocationDisplay = document.createElement("div");
  document.body.appendChild(doubleClickLocationDisplay);

  document.body.addEventListener("keypress", function onKeyPress1(event) {
    if (event.key === "c") {
      // După prima apăsare dezactivez această funcționalitate
      document.body.removeEventListener("keypress", onKeyPress1);

      // La fiecare 3 secunde, schimbăm aleator culoarea unei celule.
      let handle = setInterval(changeCellColor, 3000);

      document.body.addEventListener("keypress", function onKeyPress2(event) {
        if (event.key === "s") {
          document.body.removeEventListener("keypress", onKeyPress2);

          // Oprește-te din colorat aleator celulele
          clearInterval(handle);

          // Resetează-le culoarea de fundal la cea din CSS
          resetCellColors();
        }
      });
    }
  });
});

/** Funcții ajutătoare pentru localStorage */

function getN() {
  let value = localStorage.getItem("N");
  if (value === null) {
    value = "6";
    setN(value);
  }
  return value;
}

function setN(value) {
  localStorage.setItem("N", value);
}

/** Funcții ajutătoare pentru create componentele paginii */

function createRadioButton(name, value, checked) {
  let radioButton = document.createElement("input");
  radioButton.type = "radio";

  radioButton.name = name;
  radioButton.value = value;
  radioButton.checked = checked;

  return radioButton;
}

function createInputGroup() {
  let N = getN();
  let inputGroup = document.createElement("div");

  const radioName = "N";
  for (let i = 4; i <= 8; ++i) {
    let radioButton = createRadioButton(radioName, i, i.toString() === N);

    radioButton.addEventListener("click", function(event) {
      console.log(`Click pe radio button-ul #${i}`);
      setN(event.target.value);
      inputGroup.remove();
    });

    inputGroup.appendChild(radioButton);
  }

  return inputGroup;
}

function createCells() {
  let N = getN();
  for (let i = 0; i < N; ++i) {
    let row = document.createElement("div");
    row.style.display = "flex";

    for (let j = 0; j < N; ++j) {
      let cell = createCell(i, j);
      row.appendChild(cell);
    }
    parent.appendChild(row);
  }
}

function createCell(i, j) {
  let cell = document.createElement("div");

  cell.className = "celula";

  const CELL_SIZE = "80px";
  cell.style.width = CELL_SIZE;
  cell.style.height = CELL_SIZE;

  cell.addEventListener("dblclick", function(event) {
    console.log(`Dublu click pe celula de pe linia ${i} și coloana ${j}`);
    cell.textContent = `(${i}, ${j})`;
    doubleClickLocationDisplay.textContent = `Coordonate dublu click: ${event.x}, ${event.y}`;
    event.stopPropagation();
  });

  cell.addEventListener("click", function(event) {
    event.stopPropagation();
  });

  // Adaug în listă ca să le pot colora aleator
  cells.push(cell);

  return cell;
}

/** Funcție ajutătoare de generat numere întregi random într-un interval. */
function randomInt(a, b) {
  return Math.floor(a + Math.random() * (b - a));
}

let currentCell = 0;
function changeCellColor() {
  let N = getN();

  console.log(`Schimb culoarea celulei #${currentCell}`);
  let cell = cells[currentCell];

  let r = randomInt(0, 255);
  let g = randomInt(0, 255);
  let b = randomInt(0, 255);

  cell.style.backgroundColor = `rgb(${r}, ${g}, ${b})`;

  currentCell = (currentCell + 1) % (N * N);
}

function resetCellColors() {
  console.log("Resetez culorile celulelor");

  for (let cell of cells) {
    cell.style.backgroundColor = "";
  }
}
