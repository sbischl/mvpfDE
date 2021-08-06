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
    // depends on additional_program_info being loaded as we need some way of determining which government policies exist before this function can be called

    // This will be the array carrying the data. It should be compatible to the array returned from readcsv with the only difference that the json format omits all the keys with NA values.
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

    // Resolve promises in parallel
    await Promise.all(requests).then((result) => json_as_array = result);

    // This loop merges the loaded ddata with the details about each reform (description, sources, age etc..) form programs.json. The resulting array of objects then contains all relevant data.

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
    let csv_as_array;

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
    let datasetsLabels = [];
    // The actual dataSets that will eventually be returned
    let datasets = [];
    let i;

    // Sort by category
    csv_as_array.sort(function(a, b) {
        return categories.indexOf(a.category) - categories.indexOf(b.category);
    });

    for (i = 0; i < csv_as_array.length; i++) {
        let current_observation = csv_as_array[i];

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

    // Now the csv has been read. Apply the censoring. That means convert infinite values to some number (e.g.) which will act as infinity on the chart.
    censorValues(datasets);

    // thx @https://stackoverflow.com/questions/13304543/
    // Again sort by category
    datasets.sort(function(a, b){
        return categories.indexOf(a.label) - categories.indexOf(b.label);
    });
    datasetsLabels.sort(function(a, b){
        return categories.indexOf(a) - categories.indexOf(b);
    });
    return (datasets);
}

function censorValues(datasets) {
    let i;
    let j;
    let k;
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
    readjson(getGraphAssumptions().join("")).then(function (csv) {
        updateGraphDataSet(csv);
        openTooltipCurrentProgram();
        // We also have to update the MVPF on the right side
        updateProgramHeadLine();
    });
}

function updateProgramHeadLine() {
    // this function manipulates the HTML of the Bar Charts
    let program_data = getUnmodifiedbyIdentProgram(currently_displayed_program)
    let mvpf_to_print = program_data["mvpf"] == "Inf" ? "∞" : parseFloat(program_data["mvpf"]).toFixed(2);
    document.querySelector("#mvpfDisplay").innerHTML = `MVPF = ${mvpf_to_print}`;
}

function updateAxis(axis, value, label) {
    if (axis === "x") {
        // Update the value to plot
        mvpfChart.options.parsing.xAxisKey = value;
    }
    else if (axis === "y") {
        mvpfChart.options.parsing.yAxisKey = value;
        // If we update the y axis, we need to update the scales
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
                grid: {
                    /* Use this to make infinity line black. Commented out because the other axis draws over the infinity line causing a grey overlap
                    color: (context) => {
                        console.log("huii");
                        if (context.tick.value === infinity_cutoff + 1) {
                            return '#000000';
                        }
                        return "rgba(0, 0, 0, 0.1)";
                    }
                    */
                },
                scaleLabel: {
                    display: true,
                    labelString: yLab
                },
                suggestedMin: lower_cutoff,
                suggestedMax: infinity_cutoff + 1,
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
                grid: {
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
                suggestedMin: wtp_lower_cutoff,
                suggestedMax: wtp_upper_cutoff,
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
                grid: {
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
                suggestedMin: cost_lower_cutoff,
                suggestedMax: cost_upper_cutoff,
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
                grid: {
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
                grid: {
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
    // Update the main Chart by updating the data of each dataset
    let updatedDatasets = generateDatasets(csv_as_array);
    
    for (let i = 0; i < updatedDatasets.length; i++) {
        mvpfChart.data.datasets[i].data = updatedDatasets[i].data;
    }
    mvpfChart.update();

    // Update Bar Charts
    let range = getScalesMinMax(currently_displayed_program);

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
    let assumptionSelectBoxesIds = ["discountRateAssumption", "taxRateAssumption", "returnsToSchoolingAssumption", "co2externality", "eti"];
    let specifiedAssumptions = [];

    for (let i = 0; i < assumptionSelectBoxesIds.length; i++) {
        specifiedAssumptions.push(document.getElementById(assumptionSelectBoxesIds[i]).value);
    }
    return (specifiedAssumptions);
}

function highlightProgram(program) {
    currently_displayed_program = program;
    generateLeftSideHTMLCharts(program);
    openTooltip(program);
}

function highlightCategory(options) {
    //This function needs to get passed the options argument from the select / select picker like:
    // onchange="highlightCategory(options)"
    for (let i = 0; i < options.length; i++) {
        if (options[i].selected === false) {
            mvpfChart._metasets[i].hidden = true;
        }
        else {
            mvpfChart._metasets[i].hidden = false;
        }
    }
    mvpfChart.update();
    /*
    console.log(category);
    for (var i = 0; i < mvpfChart._metasets.length; i++) {
        var current_metaset = mvpfChart._metasets[i];
        if (current_metaset._dataset.label == category) {
            hideMetaSetsExcept(i);
            return
        }
    }
    console.log("Category to be highlighted not present in graph. Show all");
    hideMetaSetsExcept(-1);
    */
}
/*
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
*/

function openTooltipCurrentProgram() {
    if (currently_displayed_program) {
        openTooltip(currently_displayed_program);
    }
}

function openTooltip(program) {
    // Find coordinates of point:
    let coordinates;
    let i = 0
    while (true) {
        // Use infinite loop and try catch here because I could not find an easy way of determining the number of DatasetMetas. The current code looks at the next meta until an exception is thrown.
        try {
            let currentDataSetMeta = mvpfChart.getDatasetMeta(i);
            // Check if program is in this Meta:
            let j
            let current_data = Object.values(currentDataSetMeta._dataset.data)
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
    // Open Tooltip by simulating a mouse press
    let mouseMoveEvent, rectangle;
    rectangle = mvpfChart.canvas.getBoundingClientRect();

    mouseMoveEvent = new MouseEvent('mousemove', {
        clientX: rectangle.left + coordinates.x,
        clientY: rectangle.top + coordinates.y
    });
    mvpfChart.canvas.dispatchEvent(mouseMoveEvent);
}

function getUnmodifiedProgram(program_name) {
    for (let i = 0; i < unmodified_dataset.length; i++) {
        if (unmodified_dataset[i].program_name == program_name) {
            return unmodified_dataset[i];
        }
    }
    console.log("There is a problem. Program not found");
}

function getUnmodifiedbyIdentProgram(programIdent) {
    for (let j = 0; j < unmodified_dataset.length; j++) {
        if (unmodified_dataset[j].program == programIdent) {
            return unmodified_dataset[j];
        }
    }
    console.log("There is a problem. Program not found");
}


function selectColor(number, background = false) {
    // returns a string in the format rgba(123,123,123,0.6)
    let background_opa = 0.7
    let foreground_opa = 0.9
    if (number > colors.length) {
        console.log("Color not in range of supplied colors in colors.json");
        // Reuse last color in this case
        number = colors.length;
    }
    color_triple = colors[number - 1];
    return `rgba(${color_triple[0]},${color_triple[1]},${color_triple[2]},${background ? background_opa : foreground_opa})`;
}

function addAllPositivesSubtractAllNegatives(array) {
    let negative = 0;
    let positive = 0;
    let i;
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
    // we want the same scale on all of the bar plots. To ensure this we calculate the required min length of the x-axis for each plot and apply the largest to all.
    let relevant_datapoint = unmodified_dataset.find(function (datapoint) {
        return (datapoint.program === program);
    });

    let max = addAllPositivesSubtractAllNegatives([
        Math.abs(parseFloat(relevant_datapoint["willingness_to_pay"]),
            -Math.abs(parseFloat(relevant_datapoint["government_net_costs"])))
    ]);
    let mappingEntry;

    for (let i = 0; i < variable_mapping.length; i++) {
        if (variable_mapping[i].program === program) {
            mappingEntry = variable_mapping[i];
            break;
        }
    }

    barComponents = mappingEntry["willingness_to_pay"];
    let array = [];
    let component;
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
    let datasets = [];
    let barComponents;
    let relevant_datapoint = unmodified_dataset.find(function (datapoint) {
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
    for (let i = 0; i < variable_mapping.length; i++) {
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
    let range = getScalesMinMax(program);

    // Check if the screen size is small
    let smallscreen = jQuery(window).width() < 1450 ? true : false
    barChart = new Chart(chartElement, {
        type: 'bar',
        data: {
            labels: '.', // In ChartJS Beta 3.0 this could be left empty. If we leave it empty in 3.3, there is no graph drawn anymore. (And the tooltip is now shared for all areas of the chart in 3.3). 
            datasets: generateBarData(csv_as_array, variable_to_plot, program)
        },
        options: {
            indexAxis: 'y',
            responsive: true,
            maintainAspectRatio: false,
            aspectRatio: smallscreen ? 2 : 4,
            //devicePixelRatio: 4, //Set this to save a high res png. Otherwise leave default
            plugins: {
                legend: {
                    position: 'bottom',
                    usePointStyle: true,
                    onClick: () => {} //This disables the ability to click on the legend and remove effects.
                },
                tooltip: {
                    enabled: true,
                    displayColors: false,
                    bodyColor: "black",
                    backgroundColor: "white",
                    borderWidth: 2,
                    cornerRadius: 8,
                    bodyFont: {weight: 600},
                    borderColor: getComputedStyle(document.body).getPropertyValue('--primary-color'),
                    mode: 'nearest',
                    callbacks: {
                        title: function (data) {
                            return null;
                        }
                    }
                }
            },
            scales: {
                x: {
                    stacked: variable_to_plot == "mvpf" ? false : true,
                    ticks: {
                        maxTicksLimit: 3
                    },
                    grid: {
                        drawOnChartArea: false,
                        lineWidth: 0,
                        tickWidth: 1
                    },
                    min: -range,
                    max: range,
                    afterTickToLabelConversion: function(data) {
                        // this adds € signs to the tick marks on the x-axis.
                        let ticks = data.ticks;
                        for (var i = 0; i < ticks.length; i++) {
                            ticks[i].label = ticks[i].label.replace(".", "placeholder").replace(",", ".").replace("placeholder", ",") + "€";
                        }
                        return data;
                    }
                },
                y: {
                    stacked: variable_to_plot == "mvpf" ? false : true,
                    display: false
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
    let tooltip_number = 1;

    let mvpfChartElement = document.getElementById('mvpfChart');
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
            plugins: {
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
                tooltip: {
                    enabled: true,
                    displayColors: false,
                    bodyColor: "black",
                    titleColor: "black",
                    backgroundColor: "white",
                    borderWidth: 2,
                    cornerRadius: 8,
                    bodyFont: { weight: 500 },
                    borderColor: getComputedStyle(document.body).getPropertyValue('--primary-color'),
                    mode: 'point',
                    callbacks: {
                        title: function (data) {
                            if (data.length > 1) {
                                tooltip_number = data.length;
                                tooltip_counter = 1;
                                let title = data[0].dataset.data[data[0].dataIndex]["program_name"];
                                for (let i = 1; i < data.length; i++) {
                                    title += ` &\n${data[i].dataset.data[data[i].dataIndex]["program_name"]}`
                                }
                                return title;
                            }
                            tooltip_number = 1;
                            tooltip_counter = 1;
                            return data[0].dataset.data[data[0].dataIndex]["program_name"];
                        },
                        label: function (data) {
                            // Get the name of the program. Since we have censored the data to draw the graph we need to look up the true values:
                            let program_name = data.dataset.data[data.dataIndex]["program_name"];
                            let unmodified_datapoint = getUnmodifiedProgram(program_name);
                            let tooltip = [];
                            let mvpfIsInfinity = unmodified_datapoint["mvpf"] == "Inf"
    
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
            },
            hover: {
                mode: "point"
            }
        }
    });
    
    mvpfChartElement.onclick = function (evt) {
        //This is totally weird, see https://github.com/chartjs/Chart.js/issues/2292
        //But it works now!!!!!
        let activePoints = mvpfChart.getElementsAtEventForMode(evt, 'point', mvpfChart.options);
        let firstPoint = activePoints[0];
        // activePoints is undefined in case no point is pressed. if(undefined) is false, this condition prevents
        // the code from being executed if nothing is clicked.
        if (activePoints[0]) {
            let clicked_program = mvpfChart.data.datasets[firstPoint.datasetIndex].data[firstPoint.index].program;
            changeBarChartProgram(clicked_program);
        }
    };
}

function changeBarChartProgram(program) {
    currently_displayed_program = program;
    generateLeftSideHTMLCharts(program);

    // Update Highlighted Program on left side panel:
    let selection = document.querySelector('#highlightProgram');
    for (let i = 0; i < selection.options.length; i++) {
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
    let allBarCharts = [];

    let i = 0;
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
    generateSingleProgramHTML(program)

    bar_counter = 1;
    governmentCostChart = drawBarChart(unmodified_dataset, "government_net_costs", program, document.querySelector("#costgraph"));
    wtpChart = drawBarChart(unmodified_dataset, "willingness_to_pay", program, document.querySelector("#wtpgraph"));
    wtpCostChart = drawBarChart(unmodified_dataset, "mvpf", program, document.querySelector("#wtpcostgraph"));
    
    // Increase size of bar Charts if legend has to many elements:
    let chartArray = [wtpChart, governmentCostChart, wtpCostChart];
    let barChartDivArray = [document.querySelector("#wtpgraphdiv"), document.querySelector("#costgraphdiv"), document.querySelector("#wtpcostgraphdiv")]
    let screensize = jQuery(window).width();
    let effect_size = 35 * (500 / screensize);
    for (let i = 0; i < chartArray.length; i++) {
        legendItems = chartArray[i]._sortedMetasets.length;
        if (legendItems > 3) {
            barChartDivArray[i].style.height = (160 + (legendItems - 3) * effect_size) + "px" 
        }
    }
}

function generateSingleProgramHTML(program) {
    program_data = getUnmodifiedbyIdentProgram(program);
    
    
    // Generate author citiations
    let links = program_data.links.split(";");
    let authors = program_data.sources.split(";");
    let listElements = "";
    for (let i = 0; i < authors.length; i++) {
        listElements += `<li>${authors[i]} <a href="${links[i]}" class="btn linkbutton" target="_blank">LINK</a></li>`
    }

    // Generate Description, Bar Chart divs etc..
    rightHandSideDiv = document.querySelector("#rightsidebarcharts");
    rightHandSideDiv.innerHTML = `
    <h3>${program_data.program_name}<hr style="margin-bottom: 5px"><small class="text-muted" id="mvpfDisplay" style="font-style: italic;">MVPF = ${program_data["mvpf"] == "Inf" ? "∞" : parseFloat(program_data["mvpf"]).toFixed(2)}</small></h3>

            <button type="button" class="btn collapseicon" data-toggle="collapse"
              data-target="#refdescription">Description</button>
            <div id="refdescription" class="collapse show"><p>${program_data.short_description}</p></div>

            <button type="button" class="btn collapseicon" data-toggle="collapse" data-target="#costgraphdiv">Willingness
              to Pay:</button>
            <div class="barChartDiv collapse show" id="wtpgraphdiv">
            <canvas id="wtpgraph"></canvas>
            </div>

            <button type="button" class="btn collapseicon" data-toggle="collapse" data-target="#wtpgraphdiv">Government Net
              Cost:</button>
            <div class="barChartDiv collapse show" id="costgraphdiv">
            <canvas id="costgraph"></canvas>
            </div>

            <button type="button" class="btn collapseicon" data-toggle="collapse" data-target="#wtpcostgraphdiv">Government Net Cost & Willingness to Pay:</button>
            <div class="barChartDiv collapse show" id="wtpcostgraphdiv">
            <canvas id="wtpcostgraph"></canvas>
            </div>

            <button type="button" class="btn collapseicon" data-toggle="collapse"
              data-target="#relliterature">Relevant Literature:</button>
            <div id="relliterature" class="collapse show">
            <ul>

            </ul>
            ${listElements}
            </div>
      `
}

/*
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
*/

function populatePrograms() {
    // adds available programs to the programs select box
    let selection = document.querySelector('#highlightProgram');
    
    for (let i in unmodified_dataset) {
        var current_program = getUnmodifiedbyIdentProgram(unmodified_dataset[i].program);
        var option = document.createElement("option");
        option.value = current_program.program;
        option.innerHTML = current_program.program_name;
        option.setAttribute("data-subtext",current_program.category)
        selection.appendChild(option);
    }
    // The select picker needs to be updated
    jQuery('#highlightProgram').selectpicker('refresh');
}

function populateCategories() {
    // adds available categories to the categories select box
    let selection = document.querySelector('#highlightCategory');

    for (let i in categories) {
        var option = document.createElement("option");
        option.value = categories[i];
        option.innerHTML = categories[i];
        option.setAttribute("selected","")
        selection.appendChild(option);
    }
    // The select picker needs to be updated
    jQuery('#highlightCategory').selectpicker('refresh');
}

function main() {
    // Show collapse if screen size large enough:
    if (jQuery(window).width() > 960) {
        let chartOptions = document.querySelector("#chartOptions");
        chartOptions.classList.add("show");
        let assumptions = document.querySelector("#assumptions");
        assumptions.classList.add("show");
    }


    /* Load the required data mostly asyncronously.
    First load programs, and categories. readjson depends on these being already loaded.
    Then (simultaneously):
    1. readjson("default") -> load the default spec of each program
    2. the variable mapping -> loads the composition of the net cost and wtp
    3. the colors -> loads the colors used in the plot
    4. the categories -> loads the categories. The nth category is displayed in the nth color.
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