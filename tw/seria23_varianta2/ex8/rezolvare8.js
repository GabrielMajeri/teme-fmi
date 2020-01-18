window.addEventListener("load", function() {
    const div = document.getElementById("dv");
    div.addEventListener("mouseenter", function() {
        const compStyle = getComputedStyle(div);
        const left = parseInt(compStyle.getPropertyValue("left"));

        div.style.left = (left + 10) + "px";
    });

    div.addEventListener("click", function(event) {
        event.stopPropagation();
    });

    document.addEventListener("click", function(event) {
        const x = event.pageX;
        const y = event.pageY;

        div.style.left = x + "px";
        div.style.top = y + "px";
    });
});
