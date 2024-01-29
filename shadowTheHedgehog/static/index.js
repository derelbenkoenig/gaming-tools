async function fetchRoute(num) {
    return await fetch(`/api/v1/routes/${num}`);
}

function tableRowFromChoice({level, mission}) {
    const row = document.createElement("tr");
    const levelCell = document.createElement("td");
    levelCell.innerText = level;
    const missionCell = document.createElement("td");
    missionCell.innerText = mission;
    row.append(levelCell, missionCell)
    return row
}

function updateTable(choices) {
    const tableBody = document.getElementById("routeTableBody");
    const newRows = choices.map((choice) => tableRowFromChoice(choice));
    tableBody.children.forEach((child) => { child.remove(); });
    tableBody.append(...newRows);
}

document.getElementById("getRouteButton").addEventListener("click", async () => {
    const routeNum = parseInt(document.getElementById("numberInput").value);
    const routeChoices = await fetchRoute(routeNum);
    updateTable(routeChoices);
});
