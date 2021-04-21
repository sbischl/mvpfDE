 // This should contain all of the required js code



// Settings (some of these have to be in line with what the R code does that exports the csv files):
const document_root = '';
const infinity_cutoff = 6;
const lower_cutoff = -1;

const cost_lower_cutoff = -1
const cost_upper_cutoff = 3 
const wtp_lower_cutoff = -1
const wtp_upper_cutoff = 3

// Headline Fonts and Font size
const headline_font = "Source Sans Pro";
const headline_fontsize = 20;

// Generate global variables pointing to each of the chart objects to later modify those
var mvpfChart;
var governmentCostChart;
var wtpChart;
var wtpCostChart;

// Store programHeadline in global so that we can update the MVPF
var programHeadLine
//Store barChartSuperDiv's (Divs that hold the Bar Charts) in a array
var barChartSuperDivArray;
// String reference to the currently displayed program:
var currently_displayed_program;

// Bar Chart Div. This one has to be dynamically updated.
var chartDiv = document.querySelector("#barChartDiv")

// Count Tooltip calls  (required to deal with multiple programs located at the exact same location)
var tooltip_counter = 1;

// Categories (in order that they are displayed in the legend). These are loaded from data/categories.json:
var categories;

// Store the additional paramters of each policy. These are loaded from data/programs.json:
var additional_program_info

// Store the mapping of willingness to pay and government net cost in a JSON object. (loaded from data/variable_mapping)
var variable_mapping;

// Store the colors used for the Graph (loaded from data/colors.json);
var colors;

// Store a unmodified, easy to access version of all programs:
var unmodified_dataset;

// This counts the number of categories / or datasets in the chart.js context that have been added so far
var category_counter_mvpf = 0;
var bar_counter = 1;

// Simple helper function which loads a url to a json file into a js object
async function loadJSON(url) {
    var json;
    await fetch(url).then(response => {
        json = response.json();
    });
    return json;
}

async function readjson(assumption_string) {
    // depends on additional_program_info being loaded

    // This will be the array carrying the data
    var json_as_array;

    // First we need to get a list of all programs. Where the entries are strings
    var programs = additional_program_info.map(program => {
        return program.program_identifier;
    });

    // Individual json requests to all of the programs
    var requests = programs.map(program => {
        // Note: usually one would expect that this code runs the loadJSON and stores the
        // result in requests. This is not what we want, as the http requests should be
        // run in parallel for better performance. Fortunately, this is not what is happening. loadJSON is a async function. And async functions return promises. which are not automatically resolved.
        return loadJSON(document_root + "data/" + program + "/" + assumption_string + ".json");
    });

    // Resolve promises
    await Promise.all(requests).then((result) => json_as_array = result);

    for (var i = 0; i < json_as_array.length; i++) {
        Object.assign(json_as_array[i], additional_program_info[i]);
        // Rename program identifier to program. (For compatibility with the old solution)   
        json_as_array[i]["program"] = json_as_array[i]["program_identifier"];
        delete json_as_array[i]["program_identifier"];
    }

    unmodified_dataset = JSON.parse(JSON.stringify(json_as_array));

    // sort by category & ensure that categories is defined already
    if (typeof(categories) != "undefined") {
        unmodified_dataset.sort(function(a, b){
            return categories.indexOf(a.category) - categories.indexOf(b.category);
        });
    }
    return json_as_array;
}

// function to readcsv files ... no longer in use
async function readcsv(csv_location) {
    var csv_as_array;

    // This fixes the broken Umlaute: Thx @https://stackoverflow.com/questions/15333711/specials-chars-from-csv-to-javascript
    // Now this breaks them. I dunno what I have changed. These encoding problems are weird
    /*
    jQuery.ajaxSetup({
        'beforeSend' : function(xhr) {
            xhr.overrideMimeType('text/html; charset=iso-8859-1');
        }
    });
    */

    await jQuery.get(csv_location, function (data) {
        // Calling toObject. Javascript's typeof() function calls the resulting "thing" a object. But to me this looks like an array that contains key-value pairs (= objects??).
        // When calling toArrays. We would loose the key for each of the values and would have to infer the key from the position in the array.
        // -> Would be inconvenient
        csv_as_array = jQuery.csv.toObjects(data);
    });
    //This stores a copy of the dataset
    unmodified_dataset = JSON.parse(JSON.stringify(csv_as_array));

    // sort by category & ensure that categories is defined already
    if (typeof(categories) != "undefined") {
        unmodified_dataset.sort(function(a, b){
            return categories.indexOf(a.category) - categories.indexOf(b.category);
        });
    }
    return csv_as_array;
}

function generateDatasets(csv_as_array) {
    // This stores the name or in my context the type of program (i.e. something like education policy, tax reform ...) of all the datasets we need to construct
    var datasetsLabels = [];
    // The actual dataSets that will eventually be returned
    var datasets = [];
    var i;

    csv_as_array.sort(function(a, b) {
        return categories.indexOf(a.category) - categories.indexOf(b.category);
    });

    for (i = 0; i < csv_as_array.length; i++) {
        var current_observation = csv_as_array[i];

        // Check if the dataset already exists. If not add it.
        if (!datasetsLabels.includes(current_observation.category)) {
            datasetsLabels.push(current_observation.category);
            datasets.push(generateEmptyDataset(current_observation.category));
        }
        // Add current observation to the correct dataset:

        // First get the relevant dataset
        correctDataset = datasets.filter(dataset => {
            return dataset.label === current_observation.category;
        })[0];

        // Now add the observation
        correctDataset.data.push(current_observation);
    }

    // Now the csv has been read. Apply the censoring
    censorValues(datasets);

    // thx @https://stackoverflow.com/questions/13304543/
    datasets.sort(function(a, b){
        return categories.indexOf(a.label) - categories.indexOf(b.label);
    });
    datasetsLabels.sort(function(a, b){
        return categories.indexOf(a) - categories.indexOf(b);
    });
    return (datasets);
}

function censorValues(datasets) {
    var i;
    var j;
    var k;
    for (i = 0; i < datasets.length; i++) {
        current_dataset = datasets[i].data;
        for (j = 0; j < current_dataset.length; j++) {
            for (k in current_dataset[j]) {
                if (k == "mvpf") {
                    if (current_dataset[j][k] > infinity_cutoff) {
                        current_dataset[j][k] = infinity_cutoff;
                    }
                    else if (current_dataset[j][k] < lower_cutoff) {
                        current_dataset[j][k] = lower_cutoff;
                    }
                    else if (current_dataset[j][k] == "Inf") {
                        current_dataset[j][k] = infinity_cutoff + 1;
                    }
                }
                else if (k == "government_net_costs_per_program_cost") {
                    if (current_dataset[j][k] > cost_upper_cutoff ) {
                        current_dataset[j][k] = cost_upper_cutoff;
                    }
                    else if (current_dataset[j][k] < cost_lower_cutoff ) {
                        current_dataset[j][k] = cost_lower_cutoff;
                    }
                }
                else if (k == "willingness_to_pay_per_program_cost") {
                    if (current_dataset[j][k] > wtp_upper_cutoff ) {
                        current_dataset[j][k] = wtp_upper_cutoff;
                    }
                    else if (current_dataset[j][k] < wtp_lower_cutoff ) {
                        current_dataset[j][k] = wtp_lower_cutoff;
                    }
                }
            }
        }
    }
}

function generateEmptyDataset(datasetLabel) {
    category_counter_mvpf++;
    return ({
        label: datasetLabel,
        data: [],
        backgroundColor: selectColor(category_counter_mvpf, true),
        borderColor: selectColor(category_counter_mvpf),
        pointHoverRadius: 7,
        pointRadius: 5
    });
}

async function updateGraphAssumptions() {
    console.log(getGraphAssumptions().join(""));
    readjson(getGraphAssumptions().join("")).then(function (csv) {
        updateGraphDataSet(csv);
        openTooltipCurrentProgram();
        // We also have to update the MVPF on the right side
        updateProgramHeadLine();
    });
}

function updateProgramHeadLine() {
    var program_data = getUnmodifiedbyIdentProgram(currently_displayed_program)
    var mvpf_to_print = program_data["mvpf"] == "Inf" ? "∞" : parseFloat(program_data["mvpf"]).toFixed(2);
    var current_html = programHeadLine.innerHTML;
    // find pattern to replace
    var start = current_html.indexOf("MVPF = ");
    var end = current_html.indexOf("</span>");
    var toReplace = current_html.substring(start + 6, end);
    console.log(toReplace);
    var current_html = current_html.replace(toReplace, " " + mvpf_to_print);
    programHeadLine.innerHTML = current_html;
}

function updateAxis(axis, value, label) {
    if (axis === "x") {
        // Update the value to plot
        mvpfChart.options.parsing.xAxisKey = value;
        mvpfChart.options.scales = getScales(variable = value, xLab = label, ylab = mvpfChart.options.scales.y.scaleLabel.labelString);
    }
    else if (axis === "y") {
        mvpfChart.options.parsing.yAxisKey = value;
        mvpfChart.options.scales = getScales(variable = value, xLab = mvpfChart.options.scales.x.scaleLabel.labelString, ylab = label);
    }
    mvpfChart.update();
}

function getScales(variable,
    xLab = mvpfChart.options.scales.x.scaleLabel.labelString,
    yLab = mvpfChart.options.scales.y.scaleLabel.labelString) {
    if (variable == "mvpf" | yLab == "Marginal Value of Public Funds") {
        return ({
            y: {
                display: true,
                scaleLabel: {
                    display: true,
                    labelString: yLab
                },
                ticks: {
                    min: lower_cutoff,
                    max: infinity_cutoff,
                    stepSize: 1,
                    callback: function (value, index, values) {
                        if (value == infinity_cutoff + 1) {
                            return "∞";
                        }
                        else if (value == infinity_cutoff) {
                            return "≥" + infinity_cutoff;
                        }
                        else if (value == lower_cutoff) {
                            return "≤" + lower_cutoff;
                        }
                        else {
                            return value;
                        }
                    }
                }
            },
            x: {
                display: true,
                gridLines: {
                    display: false
                },
                scaleLabel: {
                    display: true,
                    labelString: xLab
                },
                ticks: {
                    maxTicksLimit: 6,
                    callback: function (value, index, values) {
                        return value;
                    }
                }
            }
        });
    }
    else if (variable == "willingness_to_pay_per_program_cost") {
        return ({
            y: {
                display: true,
                scaleLabel: {
                    display: true,
                    labelString: yLab
                },
                ticks: {
                    min: wtp_lower_cutoff,
                    max: wtp_upper_cutoff,
                    stepSize: 1,
                    callback: function (value, index, values) {
                        if (value == wtp_upper_cutoff) {
                            return "≥" + wtp_upper_cutoff;
                        }
                        else if (value == wtp_lower_cutoff) {
                            return "≤" + wtp_lower_cutoff;
                        }
                        else {
                            return value;
                        }
                    }
                }
            },
            x: {
                display: true,
                gridLines: {
                    display: false
                },
                scaleLabel: {
                    display: true,
                    labelString: xLab
                },
                ticks: {
                    maxTicksLimit: 6,
                    callback: function (value, index, values) {
                        return value;
                    }
                }
            }
        });
    }
    else if (variable == "government_net_costs_per_program_cost") {
        return ({
            y: {
                display: true,
                scaleLabel: {
                    display: true,
                    labelString: yLab
                },
                ticks: {
                    min: cost_lower_cutoff,
                    max: cost_upper_cutoff,
                    stepSize: 1,
                    callback: function (value, index, values) {
                        if (value == cost_upper_cutoff) {
                            return "≥" + cost_upper_cutoff;
                        }
                        else if (value == cost_lower_cutoff) {
                            return "≤" + cost_lower_cutoff;
                        }
                        else {
                            return value;
                        }
                    }
                }
            },
            x: {
                display: true,
                gridLines: {
                    display: false
                },
                scaleLabel: {
                    display: true,
                    labelString: xLab
                },
                ticks: {
                    maxTicksLimit: 6,
                    callback: function (value, index, values) {
                        return value;
                    }
                }
            }
        });
    }
    else {
        return ({
            y: {
                display: true,
                scaleLabel: {
                    display: true,
                    labelString: yLab
                }
            },
            x: {
                display: true,
                gridLines: {
                    display: false
                },
                scaleLabel: {
                    display: true,
                    labelString: xLab
                },
                ticks: {
                    maxTicksLimit: 6,
                    callback: function (value, index, values) {
                        return value;
                    }
                }
            }
        });
    }
}

function updateGraphDataSet(csv_as_array) {
    // Update the main Chart
    var updatedDatasets = generateDatasets(csv_as_array);
    var i;
    for (i = 0; i < updatedDatasets.length; i++) {
        mvpfChart.data.datasets[i].data = updatedDatasets[i].data;
    }
    mvpfChart.update();
    // Update Bar Charts
    var range = getScalesMinMax(currently_displayed_program);

    if (governmentCostChart) {
        updatedDatasets = generateBarData(csv_as_array, "government_net_costs", currently_displayed_program);
        for (i = 0; i < updatedDatasets.length; i++) {
            governmentCostChart.data.datasets[i].data = updatedDatasets[i].data;
        }
        governmentCostChart.options.scales.x.min = -range;
        governmentCostChart.options.scales.x.max = range;
        governmentCostChart.update();
    }
    if (wtpChart) {
        updatedDatasets = generateBarData(csv_as_array, "willingness_to_pay", currently_displayed_program);
        for (i = 0; i < updatedDatasets.length; i++) {
            wtpChart.data.datasets[i].data = updatedDatasets[i].data;
        }
        wtpChart.options.scales.x.min = -range;
        wtpChart.options.scales.x.max = range;
        wtpChart.update();
    }
    if (wtpCostChart) {
        updatedDatasets = generateBarData(csv_as_array, "mvpf", currently_displayed_program);
        for (i = 0; i < updatedDatasets.length; i++) {
            wtpCostChart.data.datasets[i].data = updatedDatasets[i].data;
        }
        wtpCostChart.options.scales.x.min = -range;
        wtpCostChart.options.scales.x.max = range;
        wtpCostChart.update();
    }
}

function getCSVLocation(specifiedAssumptions) {
    return (document_root + "/csv/" + specifiedAssumptions.join("") + ".csv");
}

function getGraphAssumptions() {
    // The assumption Select Box Ids have to be in the correct order!!
    // The correct order is given by order of the assumptions in the csv / json file names
    var assumptionSelectBoxesIds = ["discountRateAssumption", "taxRateAssumption", "returnsToSchoolingAssumption", "valueOfStatisticalLifeAssumption", "co2externality", "wage_growth_rate", "eti"];
    var specifiedAssumptions = [];

    var i;
    for (i = 0; i < assumptionSelectBoxesIds.length; i++) {
        specifiedAssumptions.push(document.getElementById(assumptionSelectBoxesIds[i]).value);
    }
    return (specifiedAssumptions);
}

function highlightProgram(program) {
    currently_displayed_program = program;
    generateLeftSideHTMLCharts(program);
    openTooltip(program);
}

function highlightCategory(category) {
    for (var i = 0; i < mvpfChart._metasets.length; i++) {
        var current_metaset = mvpfChart._metasets[i];
        if (current_metaset._dataset.label == category) {
            hideMetaSetsExcept(i);
            return
        }
    }
    console.log("Category to be highlighted not present in graph. Show all");
    hideMetaSetsExcept(-1);
}

function hideMetaSetsExcept(number) {
    // if number is positive, hides all metasets in the graph except number. If negative hides none, but rather makes all visiable
    for (var i = 0; i < mvpfChart._metasets.length; i++) {
        var current_metaset = mvpfChart._metasets[i];
        if (number < 0) {
            current_metaset.hidden = false;
            continue;
        }
        if (i == number) {
            current_metaset.hidden = false;
        }
        else {
            current_metaset.hidden = true;
        }
    }
    mvpfChart.update();
}

function openTooltipCurrentProgram() {
    if (currently_displayed_program) {
        openTooltip(currently_displayed_program);
    }
}

function openTooltip(program) {
    // Find coordinates of point:
    var coordinates;
    var i = 0
    while (true) {
        try {
            var currentDataSetMeta = mvpfChart.getDatasetMeta(i);
            // Check if program is in this Meta:
            var j
            var current_data = Object.values(currentDataSetMeta._dataset.data)
            for (j = 0; j < current_data.length; j++) {
                if (current_data[j].program === program) {
                    coordinates = currentDataSetMeta.data[j].getCenterPoint();
                }
            }
            i++;
        }
        catch (e) {
            i++;
            break;
        }
    }

    // Thx; @jwerre https://stackoverflow.com/questions/39283177/programmatically-open-and-close-chart-js-tooltip
    var mouseMoveEvent, rectangle;
    rectangle = mvpfChart.canvas.getBoundingClientRect();

    mouseMoveEvent = new MouseEvent('mousemove', {
        clientX: rectangle.left + coordinates.x,
        clientY: rectangle.top + coordinates.y
    });
    mvpfChart.canvas.dispatchEvent(mouseMoveEvent);

}

function getUnmodifiedProgram(program_name) {
    var i;
    for (i = 0; i < unmodified_dataset.length; i++) {
        if (unmodified_dataset[i].program_name == program_name) {
            return unmodified_dataset[i];
        }
    }
    console.log("There is a problem. Program not found");
}

function getUnmodifiedbyIdentProgram(programIdent) {
    var j;
    for (j = 0; j < unmodified_dataset.length; j++) {
        if (unmodified_dataset[j].program == programIdent) {
            return unmodified_dataset[j];
        }
    }
    console.log("There is a problem. Program not found");
}


function selectColor(number, background = false) {
    background_opa = 0.7
    foreground_opa = 0.9
    if (number > colors.length) {
        console.log("Color not in range of supplied colors in colors.json");
        // Reuse last color in this case
        number = colors.length;
    }
    color_triple = colors[number - 1];
    return `rgba(${color_triple[0]},${color_triple[1]},${color_triple[2]},${background ? background_opa : foreground_opa})`;
}

function addAllPositivesSubtractAllNegatives(array) {
    var negative = 0;
    var positive = 0;
    var i;
    for (i = 0; i < array.length; i++) {
        if (array[i] > 0) {
            positive += array[i];
        }
        else {
            negative -= array[i];
        }
    }
    return (Math.max(Math.abs(positive), Math.abs(negative)));
}

function getScalesMinMax(program) {
    var relevant_datapoint = unmodified_dataset.find(function (datapoint) {
        return (datapoint.program === program);
    });
    var max = addAllPositivesSubtractAllNegatives([
        Math.abs(parseFloat(relevant_datapoint["willingness_to_pay"]),
            -Math.abs(parseFloat(relevant_datapoint["government_net_costs"])))
    ]);
    var mappingEntry;

    var i;
    for (i = 0; i < variable_mapping.length; i++) {
        if (variable_mapping[i].program === program) {
            mappingEntry = variable_mapping[i];
            break;
        }
    }

    barComponents = mappingEntry["willingness_to_pay"];
    var array = [];
    var component;
    for (component in barComponents) {
        array.push(parseFloat(relevant_datapoint[component]));
    }
    max = Math.max(addAllPositivesSubtractAllNegatives(array), max);


    barComponents = mappingEntry["government_net_costs"];
    array = [];
    for (component in barComponents) {
        array.push(parseFloat(relevant_datapoint[component]));
    }
    max = Math.max(addAllPositivesSubtractAllNegatives(array), max);

    return (max);
}

function generateBarData(csv_as_array, variable_to_plot, program) {
    var datasets = [];
    var barComponents;
    var relevant_datapoint = unmodified_dataset.find(function (datapoint) {
        return (datapoint.program === program);
    });

    if (variable_to_plot === "mvpf") {
        datasets.push({
            label: "Government Net Cost",
            data: [parseFloat(relevant_datapoint["government_net_costs"])],
            backgroundColor: selectColor(bar_counter, true),
            borderColor: selectColor(bar_counter),
        });
        datasets.push({
            label: "Willingness to Pay",
            data: [relevant_datapoint["willingness_to_pay"]],
            backgroundColor: selectColor(bar_counter + 1, true),
            borderColor: selectColor(bar_counter + 1),
        });
        bar_counter += 2;
        return (datasets);
    }
    var i;
    for (i = 0; i < variable_mapping.length; i++) {
        if (variable_mapping[i].program === program) {
            barComponents = variable_mapping[i][variable_to_plot];
            break;
        }
    }

    for (component in barComponents) {
        datasets.push({
            label: barComponents[component],
            data: [relevant_datapoint[component]],
            backgroundColor: selectColor(bar_counter, true),
            borderColor: selectColor(bar_counter)
        });
        bar_counter++;
    }
    return (datasets);
}

function drawBarChart(csv_as_array, variable_to_plot, program, chartElement) {
    currently_displayed_program = program;
    // Set Font size
    Chart.defaults.font.size = 15.5;
    Chart.defaults.font.family = 'Open Sans';

    // Get Plotting range
    var range = getScalesMinMax(program);

    var smallscreen = jQuery(window).width() < 1450 ? true : false
    barChart = new Chart(chartElement, {
        type: 'bar',
        data: {
            datasets: generateBarData(csv_as_array, variable_to_plot, program)
        },
        options: {
            indexAxis: 'y',
            responsive: true,
            maintainAspectRatio: false,
            aspectRatio: smallscreen ? 2 : 4,
            //devicePixelRatio: 4, //Set this to save a high res png. Otherwise leave default
            legend: {
                position: 'bottom',
                usePointStyle: true,
                onClick: () => {} //This disables the ability to click on the legend and remove effects.
            },
            scales: {
                x: {
                    stacked: variable_to_plot == "mvpf" ? false : true,
                    ticks: {
                        maxTicksLimit: 3
                    },
                    gridLines: {
                        drawOnChartArea: false
                    },
                    min: -range,
                    max: range,
                    afterTickToLabelConversion: function(data) {
                        // this adds € signs to the tick marks on the x-axis.
                        ticks = data.ticks;
                        for (var i = 0; i < ticks.length; i++) {
                            ticks[i].label = ticks[i].label.replace(".", "placeholder").replace(",", ".").replace("placeholder", ",") + "€";
                        }
                        return data;
                    }
                },
                y: {
                    stacked: variable_to_plot == "mvpf" ? false : true,
                    gridLines: {
                        drawBorder: false,
                        display: false
                    }
                }
            },
            tooltips: {
                enabled: true,
                displayColors: false,
                mode: 'nearest',
                callbacks: {
                    title: function (data) {
                        return null;
                    }
                }
            }
        }
    });
    return (barChart);
}

function drawMVPFChart(csv_as_array) {
    // Set Font size
    Chart.defaults.font.size = 15.5;
    Chart.defaults.font.family = 'Open Sans';

    // Number Of Programs at Tooltip.
    var tooltip_number = 1;

    var mvpfChartElement = document.getElementById('mvpfChart');
    mvpfChart = new Chart(mvpfChartElement, {
        type: 'scatter',
        data: {
            datasets: generateDatasets(csv_as_array)
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            aspectRatio: 1.4,
            parsing: {
                yAxisKey: 'mvpf',
                xAxisKey: 'year'
            },
            scales: getScales("mvpf", "Year", "Marginal Value of Public Funds"),
            legend: {
                position: 'bottom',
                usePointStyle: true,
                labels: {
                    filter: function(legendItem, chartData) {
                        //return true;
                        if (legendItem.text == "Other") {
                            return false;
                        }
                        else {
                            return true;
                        }
                    }
                }
            },
            hover: {
                mode: "point"
            },
            tooltips: {
                enabled: true,
                displayColors: false,
                mode: 'point',
                callbacks: {
                    title: function (data) {
                        if (data.length > 1) {
                            tooltip_number = data.length;
                            tooltip_counter = 1;
                            return null;
                        }
                        tooltip_number = 1;
                        tooltip_counter = 1;
                        return data[0].dataset.data[data[0].dataIndex]["program_name"];
                    },
                    label: function (data) {
                        // Get the name of the program. Since we have censored the data to draw the graph we need to look up the true values:
                        var program_name = data.dataset.data[data.dataIndex]["program_name"];
                        var unmodified_datapoint = getUnmodifiedProgram(program_name);
                        var tooltip = [];
                        var mvpfIsInfinity = unmodified_datapoint["mvpf"] == "Inf" ? true : false

                        if (tooltip_number > 1) {
                            tooltip.push(program_name + ":");
                        }

                        tooltip.push("Willingness to Pay: " + +parseFloat(unmodified_datapoint["willingness_to_pay"]).toFixed(2) + "€");
                        tooltip.push("Government Net Cost: " + +parseFloat(unmodified_datapoint["government_net_costs"]).toFixed(2) + "€");
                        tooltip.push("MVPF: " + (mvpfIsInfinity ? "∞" : +parseFloat(unmodified_datapoint["mvpf"]).toFixed(2)));

                        if (tooltip_number > 1 & tooltip_counter < tooltip_number) {
                            tooltip.push("");
                        }
                        tooltip_counter++;
                        return tooltip;
                    }
                }
            }
        }
    });
    
    mvpfChartElement.onclick = function (evt) {
        //This is totally weird, see https://github.com/chartjs/Chart.js/issues/2292
        //But it works now!!!!!
        var activePoints = mvpfChart.getElementsAtEventForMode(evt, 'point', mvpfChart.options);
        var firstPoint = activePoints[0];
        // activePoints is undefined in case no point is pressed. if(undefined) is false, this condition prevents
        // the code from being executed if nothing is clicked.
        if (activePoints[0]) {
            var clicked_program = mvpfChart.data.datasets[firstPoint.datasetIndex].data[firstPoint.index].program;
            changeBarChartProgram(clicked_program);
        }
    };
}

function changeBarChartProgram(program) {
    currently_displayed_program = program;
    generateLeftSideHTMLCharts(program);

    // Update Highlighted Program on left side panel:
    var selection = document.querySelector('#highlightProgram');
    for (var i = 0; i < selection.options.length; i++) {
        if (selection.options[i].value == program) {
            selection.selectedIndex = i;
        }
    }
    /*
    currently_displayed_program = program;

    bar_counter = 1;
    var range = getScalesMinMax(program);

    governmentCostChart.data.datasets = generateBarData(unmodified_dataset, "government_net_costs", program);
    governmentCostChart.options.scales.x.min = -range;
    governmentCostChart.options.scales.x.max = range;
    governmentCostChart.update();

    wtpChart.data.datasets = generateBarData(unmodified_dataset, "willingness_to_pay", program);
    wtpChart.options.scales.x.min = -range;
    wtpChart.options.scales.x.max = range;
    wtpChart.update();

    wtpCostChart.data.datasets = generateBarData(unmodified_dataset, "mvpf", program);
    wtpCostChart.options.scales.x.min = -range;
    wtpCostChart.options.scales.x.max = range;
    wtpCostChart.update();
    */
}

//Dynamic Page Generation:
function generateProgramsHTML() {
    // This function can generate plots for all programs and add them to an html div element. This turned out to be complicated. 
    // I think I found a fix for the aspect ratio problem. But I like the simplicity of the current solution.
    //var allProgramsDiv = document.querySelector("#allProgram");
    var allBarCharts = [];

    var i = 0;
    for (i = 0; i < variable_mapping.length; i++) {
        var current_program = variable_mapping[i].program;
        singleHTML = generateSingleProgramHTML(current_program);
        allProgramsDiv.appendChild(singleHTML.singleProgramDiv);
        bar_counter = 1;
        drawBarChart(unmodified_dataset, "government_net_costs", current_program, singleHTML.chartElements[0]);
        drawBarChart(unmodified_dataset, "willingness_to_pay", current_program, singleHTML.chartElements[1]);
        drawBarChart(unmodified_dataset, "mvpf", current_program, singleHTML.chartElements[2]);
        allBarCharts.push(singleHTML.chartElements);
    }

}

function generateLeftSideHTMLCharts(program) {
    chartDiv.innerHTML = "";
    bar_counter = 1;
    var html = generateSingleProgramHTML(program);
    chartDiv.appendChild(html.singleProgramDiv);
    governmentCostChart = drawBarChart(unmodified_dataset, "government_net_costs", program, html.chartElements[0]);
    wtpChart = drawBarChart(unmodified_dataset, "willingness_to_pay", program, html.chartElements[1]);
    wtpCostChart = drawBarChart(unmodified_dataset, "mvpf", program, html.chartElements[2]);
    
    // Increase size of bar Charts if legend has to many elements:
    var chartArray = [governmentCostChart, wtpChart, wtpCostChart];
    var i;
    var screensize = jQuery(window).width();
    var effect_size = 35 * (500 / screensize);
    for (i = 0; i < chartArray.length; i++) {
        legendItems = chartArray[i]._sortedMetasets.length;
        if (legendItems > 3) {
            barChartSuperDivArray[i].style.height = (160 + (legendItems - 3) * effect_size) + "px" 
        }
    }
}

function generateSingleProgramHTML(program) {
    program_data = getUnmodifiedbyIdentProgram(program);

    var singleProgramDiv = document.createElement('div');
    singleProgramDiv.className = "singleProgramDiv";

    // Headline
    programHeadLine = document.createElement('h3');
    programHeadLine.className = "programHeadLine";
    programHeadLine.textContent = program_data.program_name ;
    var mvpf_to_print = program_data["mvpf"] == "Inf" ? "∞" : parseFloat(program_data["mvpf"]).toFixed(2);
    programHeadLine.innerHTML = programHeadLine.innerHTML + "<br><span style=\"font-weight: 300; font-family:Roboto; font-style: italic; font-size: 19.5px\">MVPF = " + mvpf_to_print  + "</span>"
    singleProgramDiv.appendChild(programHeadLine);

    //Description
    var programDescription = document.createElement('p');
    programHeadLine.className = "programDescription";
    programDescription.innerHTML = "<span style=\"font-family: "+ headline_font + "; font-size:"+ headline_fontsize + "px;\"><strong>Short Description:</strong></span> <br> <span style=\"color:rgb(102,102,102);\">" + program_data.short_description + "</span>";
    singleProgramDiv.appendChild(programDescription);

    var gccHeadline = document.createElement('div');
    gccHeadline.className = "chartHeadline";
    gccHeadline.innerHTML = "<strong><span style=\"font-family: "+ headline_font + "; font-size:"+ headline_fontsize + "px;\">Government Net Cost:</strong></span>";
    singleProgramDiv.appendChild(gccHeadline);

    var gccDiv = document.createElement('div');
    gccDiv.className = "barChartSuperDiv";
    gccDiv.innerHTML = "";
    var governmentCostChartElement = document.createElement('canvas');
    governmentCostChartElement.className = "barPlotElement";
    gccDiv.appendChild(governmentCostChartElement);
    singleProgramDiv.appendChild(gccDiv);

    var wtpHeadline = document.createElement('div');
    wtpHeadline.className = "chartHeadline";
    wtpHeadline.innerHTML = "<strong><span style=\"font-family: "+ headline_font + "; font-size:"+ headline_fontsize + "px;\">Willingness to Pay:</strong></span>";
    singleProgramDiv.appendChild(wtpHeadline);

    var wtpDiv = document.createElement('div');
    wtpDiv.className = "barChartSuperDiv";
    wtpDiv.innerHTML = "";
    var wtpChartElement = document.createElement('canvas');
    wtpChartElement.className = "barPlotElement";
    wtpDiv.appendChild(wtpChartElement);
    singleProgramDiv.appendChild(wtpDiv);

    var mvpfHeadline = document.createElement('div');
    mvpfHeadline.className = "chartHeadline";
    mvpfHeadline.innerHTML = "<strong><span style=\"font-family: "+ headline_font + "; font-size:"+ headline_fontsize + "px;\">Government Net Cost & Willingness to Pay:</strong></span>";;
    singleProgramDiv.appendChild(mvpfHeadline);
    
    var wtpCostDiv = document.createElement('div');
    wtpCostDiv.className = "barChartSuperDiv";
    wtpCostDiv.innerHTML = "";
    var wtpCostChartElement = document.createElement('canvas');
    wtpCostChartElement.className = "barPlotElement";
    wtpCostDiv.appendChild(wtpCostChartElement);
    singleProgramDiv.appendChild(wtpCostDiv);

    // I need to access the barChartSuperDivs later on to manipulate the size of the barCharts
    barChartSuperDivArray = [gccDiv, wtpDiv, wtpCostDiv];

    // Add note below:
    var note = document.createElement('p');
    note.className = "programDescription";
    note.innerHTML = "<span style=\"font-family: "+ headline_font + "; font-size:"+ headline_fontsize + "px;\"><strong>Note:</strong></span> <span style=\"color: rgb(102,102,102);\"> All calculations are in 2010 euro prices";
    singleProgramDiv.appendChild(note);

    //Cite papers
    var html = "<span style=\"font-family: "+ headline_font + "; font-size:"+ headline_fontsize + "px;\"><strong>Relevant Literature:</strong></span> <br>"
    var links = program_data.links.split(";");
    var authors = program_data.sources.split(";");
    var i;
    for (i = 0; i < authors.length; i++) {
        html += "<span style=\"color: rgb(102,102,102);\">" + 
        authors[i] + 
        ', <a href="' + links[i] + 
        '" target="_blank">[Link]</a>' + 
        " <br>";
    }

    var sources = document.createElement('p');
    sources.className = "programDescription";
    sources.innerHTML = html;
    singleProgramDiv.appendChild(sources);

    return {
        singleProgramDiv: singleProgramDiv,
        chartElements: [governmentCostChartElement, wtpChartElement, wtpCostChartElement]
    };
}

function populatePrograms() {
    var selection = document.querySelector('#highlightProgram');
    var i;
    for (i in variable_mapping) {
        var current_program = getUnmodifiedbyIdentProgram(variable_mapping[i].program);
        var option = document.createElement("option");
        option.value = current_program.program;
        option.innerHTML = current_program.program_name;
        selection.appendChild(option);
    }
}

function populateCategories() {
    var selection = document.querySelector('#highlightCategory');
    var i;
    for (i in categories) {
        var option = document.createElement("option");
        option.value = categories[i];
        option.innerHTML = categories[i];
        selection.appendChild(option);
    }
}

function main() {
    /* Load the required data asyncronously. That is, load:
    1. readcsv(document_root.concat("/csv/default.csv"))
    2. the variable mapping
    3. the colors
    4. the categories
    */
    Promise.all([
        loadJSON(document_root + "data/programs.json"),
        loadJSON(document_root + "data/categories.json")
    ]).then(return_values => {
        additional_program_info = return_values[0];
        categories = return_values[1];
        return Promise.all([
            readjson("default"),
            loadJSON(document_root + "data/variable_mapping.json"),
            loadJSON(document_root + "data/colors.json")
        ]);
    }).then((return_values) => {
        csv = return_values[0];
        variable_mapping = return_values[1];
        colors = return_values[2];
        drawMVPFChart(csv);
        populatePrograms();
        populateCategories();
    });
}

main();