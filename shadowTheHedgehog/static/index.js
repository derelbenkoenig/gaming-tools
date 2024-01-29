async function fetchRoute(num) {
    const resp = await fetch(`/api/v1/routes/${num}`);
    const respJson = await resp.json();
    return respJson;
}

function tableRowFromChoice({level, mission}) {
    const row = document.createElement("tr");
    const levelCell = document.createElement("td");
    levelCell.innerText = level;
    const missionCell = document.createElement("td");
    missionCell.innerText = mission;
    row.append(levelCell, missionCell)
    return row;
}

function updateTable(choices) {
    const tableBody = document.getElementById("routeTableBody");
    const newRows = choices.map((choice) => tableRowFromChoice(choice));
    for (const child of tableBody.querySelectorAll("tr")) {
        child.remove();
    }
    tableBody.append(...newRows);
}

document.addEventListener("DOMContentLoaded", () => {
    document.getElementById("getRouteButton").addEventListener("click", async (event) => {
        const routeNum = parseInt(document.getElementById("numberInput").value);
        const routeChoices = await fetchRoute(routeNum);
        updateTable(routeChoices);
        event.stopPropagation();
    });
});
