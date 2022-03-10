// Settings (some of these have to be in line with what the R code does that exports the csv files):
// Set Font size
Chart.defaults.font.size = 15.5;
Chart.defaults.font.family = 'Open Sans';

// Headline Fonts and Font size
const headline_font = "Source Sans Pro";
const headline_fontsize = 20;

// Generate global variables pointing to each of the chart objects to later modify those
var mvpfChart;
var mvpfCIChart;
var governmentCostChart;
var wtpChart;
var wtpCostChart;

// Store programHeadline in global var so that we can update the MVPF
var programHeadLine;
// String reference to the currently displayed program:
var currently_displayed_program;
// Reference to the currently displayed Chart
var currently_displayed_chart;

// Bar Chart Div. This one has to be dynamically updated.
var chartDiv = document.querySelector("#barChartDiv")

// Count Tooltip calls  (required to deal with multiple programs located at the exact same location)
var tooltip_counter = 1;

// Contains a reference to a color index which is passed around and incremented when another color is used in the bar charts.
colorholder = {
    color: 1
}

// This counts the number of categories / or datasets in the chart.js context that have been added so far
var bar_counter = 1;

// HTML Legend plugin for Chart.js (see Chart.js documentation)
const getOrCreateLegendList = (chart, id) => {
    const legendContainer = document.getElementById(id);
    let listContainer = legendContainer.querySelector('ul');

    if (!listContainer) {
        listContainer = document.createElement('ul');
        listContainer.style.display = 'flex';
        listContainer.style.flexDirection = 'row';
        listContainer.style["flex-wrap"] = "wrap";
        listContainer.style["justify-content"] = "center";
        listContainer.style.margin = 0;
        listContainer.style.padding = 0;

        legendContainer.appendChild(listContainer);
    }

    return listContainer;
};

const htmlLegendPlugin = {
    // This implements an HTML Legend. Adapted from the HTML reference of the chart.js documentation: https://www.chartjs.org/docs/master/samples/legend/html.html
    // The major advantage of this approach is that it gives separate control of the size of the graph (without the legend) and the legend itself. When the chart is drawn on the canvas element as in the chartjs standard case with the legend as part of the canvas, the number of displayed effects causes the width of the bars to vary because the height of the legend varies (More effects to display -> more elements on the legend). This caused parts of the chart and the legend to be outside of the visible area within the canvas and also created visually distracting inconsistencies between charts that should ideally look the same.  
    id: 'htmlLegend',
    afterUpdate(chart, args, options) {
        const ul = getOrCreateLegendList(chart, options.containerID);

        // Remove old legend items
        while (ul.firstChild) {
            ul.firstChild.remove();
        }

        // Reuse the built-in legendItems generator
        const items = chart.options.plugins.legend.labels.generateLabels(chart);

        //Legend entries:
        let entries = {};

        items.forEach(item => {
            // With the CI Chart, the same entries show up twice if we do not prevent it.
            if (item.text in entries) {
                return
            }
            else {
                entries[item.text] = 1
            }

            const li = document.createElement('li');
            li.style.alignItems = 'center';
            li.style.cursor = 'pointer';
            li.style.display = 'flex';
            li.style.flexDirection = 'row';
            li.style.marginLeft = '10px';

            li.onclick = () => {
                if (chart == wtpCostChart) {
                    // Disable on-click for the combined wtp and cost chart. Disabling cost or wtp does not make sense.
                    return;
                }
                const { type } = chart.config;
                if (type === 'pie' || type === 'doughnut') {
                    // Pie and doughnut charts only have a single dataset and visibility is per item
                    chart.toggleDataVisibility(item.index);
                } 
                else {
                    chart.setDatasetVisibility(item.datasetIndex, !chart.isDatasetVisible(item.datasetIndex));
                }
                if (chart == mvpfChart) {
                    // If the mvpfChart is clicked on toggling visibility and updating the chart is sufficient.
                    chart.update();
                    return;
                }
                
                // The following disables the effect that was clicked on. This persists until the page is reloaded.
                if (type === 'bar') {
                    // Determine variable of disabled effect:
                    let cost_or_wtp = governmentCostChart == chart ? "government_net_costs" : "willingness_to_pay";
                    let relevant_variable_mapping = getVariableMapping(currently_displayed_program)[cost_or_wtp];
                    for (let variable of Object.keys(relevant_variable_mapping)) {
                        if (relevant_variable_mapping[variable] === item.text) {
                            if (disabled_effects[currently_displayed_program][cost_or_wtp].includes(variable)) {
                                // Linear search here but small number of disabled effects per reform
                                disabled_effects[currently_displayed_program][cost_or_wtp].splice(disabled_effects[currently_displayed_program][cost_or_wtp].indexOf(variable), 1);
                            }
                            else {
                                disabled_effects[currently_displayed_program][cost_or_wtp].push(variable);
                            }
                        }
                    }
                    // Update Graphs to represent the disabled components.
                    updateGraphDataSet(json_as_array_copy);
                    updateProgramHeadLine();
                    setTimeout(function () {
                        openTooltipCurrentProgram()
                      }, 500);
                }
            };

            // Styling of the HTML legend goes here:
            // Color box
            const boxSpan = document.createElement('span');
            boxSpan.style.background = item.fillStyle;
            boxSpan.style.borderColor = item.strokeStyle;
            boxSpan.style.borderWidth = item.lineWidth + 'px';
            boxSpan.style.display = 'inline-block';
            boxSpan.style.height = '15px';
            boxSpan.style.marginRight = '5px';
            boxSpan.style["border-radius"] = '4px';
            boxSpan.style.width = '30px';

            // Text
            const textContainer = document.createElement('p');
            textContainer.style.color = item.fontColor;
            textContainer.style.margin = 0;
            textContainer.style.padding = 0;
            textContainer.style.textDecoration = item.hidden ? 'line-through' : '';

            const text = document.createTextNode(item.text);
            textContainer.appendChild(text);

            li.appendChild(boxSpan);
            li.appendChild(textContainer);
            ul.appendChild(li);
        });
    }
};

// function to readcsv files ... no longer in use
/*
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
    *//*

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
*/

async function updateGraphAssumptions() {
    readjson(getGraphAssumptions().join("")).then(function (json_as_array) {
        updateGraphDataSet(json_as_array);
        openTooltipCurrentProgram();
        // We also have to update the MVPF on the right side
        updateProgramHeadLine();
    });
}

function activeScatter() {
    let button = document.getElementById('select_scatter_chart');
    if (button.classList.contains("active")) {
        // Scatter Chart already active no need to do andything
        return;
    }
    document.getElementById('select_list_chart').classList.remove("active");
    button.classList.add("active");
    // Reset height to default:
    let mvpfChartDiv = document.getElementById('mvpfChartDiv');
    mvpfChartDiv.style.height = `75vh`

    document.querySelector("#chartOptions").classList.add("show");
    mvpfCIChart.destroy();
    drawMVPFChart(json_as_array_copy);
}

function activeLine() {
    let button = document.getElementById('select_list_chart');
    if (button.classList.contains("active")) {
        // Scatter Chart already active no need to do andything
        return;
    }
    document.getElementById('select_scatter_chart').classList.remove("active");
    button.classList.add("active");

    document.querySelector("#chartOptions").classList.remove("show");
    mvpfChart.destroy();
    drawListCIChart(json_as_array_copy);
}

function updateProgramHeadLine() {
    // this function manipulates the HTML of the Bar Charts
    let updated_mvpf_related_values = applyDisableEffects(currently_displayed_program)
    // We cannot display the confidence interval if some of the effects were disabled.
    let confidence_intervall = "";
    if (unmodified_dataset[currently_displayed_program]["mvpf_95ci_lower"]) {
        confidence_intervall = updated_mvpf_related_values.effect_disabled ? "" : `, 95% CI [${mvpfToString(unmodified_dataset[currently_displayed_program]["mvpf_95ci_lower"])}, ${mvpfToString(unmodified_dataset[currently_displayed_program]["mvpf_95ci_upper"])}]`;
    }
    let mvpf_to_print = mvpfToString(updated_mvpf_related_values.mvpf);
    document.querySelector("#mvpfDisplay").innerHTML = `MVPF = ${mvpf_to_print}${confidence_intervall}`;
}

function updateAxis(axis, value, label) {
    if (axis === "x") {
        // Update the value to plot
        mvpfChart.options.parsing.xAxisKey = value;
    }
    else if (axis === "y") {
        mvpfChart.options.parsing.yAxisKey = value;
        // If we update the y axis, we need to update the scales
        mvpfChart.options.scales = getScales(variable = value, xLab = mvpfChart.options.scales.x.title.text, ylab = label);
    }
    mvpfChart.update();
}

function MVPFtickCallback (value, index, values) {
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

function getScales(variable, xLab = mvpfChart.options.scales.x.title.text, yLab = mvpfChart.options.scales.y.title.text) {
    if (variable == "mvpf" | yLab == "Marginal Value of Public Funds") {
        return ({
            y: {
                display: true,
                grid: {
                    color: (context) => {
                        if (context.tick.value === infinity_cutoff + 1) {
                            return '#000000';
                        }
                        return "rgba(0, 0, 0, 0.1)";
                    }
                },
                title: {
                    display: true,
                    text: yLab
                },
                suggestedMin: lower_cutoff,
                suggestedMax: infinity_cutoff + 1,
                ticks: {
                    min: lower_cutoff,
                    max: infinity_cutoff,
                    stepSize: 1,
                    callback: MVPFtickCallback
                }
            },
            x: {
                display: true,
                grid: {
                    display: false
                },
                title: {
                    display: true,
                    text: xLab
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
                title: {
                    display: true,
                    text: yLab
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
                title: {
                    display: true,
                    text: xLab
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
                title: {
                    display: true,
                    text: yLab
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
                title: {
                    display: true,
                    text: xLab
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
                title: {
                    display: true,
                    text: yLab
                }
            },
            x: {
                display: true,
                grid: {
                    display: false
                },
                title: {
                    display: true,
                    text: xLab
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
    let updatedDatasets
    if (currently_displayed_chart == mvpfCIChart) {
        updatedDatasets = [...generateAllProgramsDatasets(json_as_array, "listci_pe"), ...generateAllProgramsDatasets(json_as_array, "listci_ci")];
        for (let i = 0; i < updatedDatasets.length; i++) {
            currently_displayed_chart.data.datasets[i].data = updatedDatasets[i].data;
        }
        currently_displayed_chart.update();
    }

    if (currently_displayed_chart == mvpfChart) {
        updatedDatasets = generateAllProgramsDatasets(csv_as_array, dataset_type = "scatter");
        for (let i = 0; i < updatedDatasets.length; i++) {
            currently_displayed_chart.data.datasets[i].data = updatedDatasets[i].data;
        }
        currently_displayed_chart.update();
    }
    
    // Update Bar Charts
    let range = getScalesMinMax(currently_displayed_program);
    // Reset Colors because all Datasets are generated again.
    colorholder.color = 1;

    if (governmentCostChart) {
        updatedDatasets = generateBarChartProgramData("government_net_costs", currently_displayed_program, colorholder);
        for (i = 0; i < updatedDatasets.length; i++) {
            governmentCostChart.data.datasets[i].data = updatedDatasets[i].data;
        }
        governmentCostChart.options.scales.x.min = -range;
        governmentCostChart.options.scales.x.max = range;
        governmentCostChart.update();
    }
    if (wtpChart) {
        updatedDatasets = generateBarChartProgramData("willingness_to_pay", currently_displayed_program,colorholder);
        for (i = 0; i < updatedDatasets.length; i++) {
            wtpChart.data.datasets[i].data = updatedDatasets[i].data;
        }
        wtpChart.options.scales.x.min = -range;
        wtpChart.options.scales.x.max = range;
        wtpChart.update();
    }
    if (wtpCostChart) {
        updatedDatasets = generateBarChartProgramData("mvpf", currently_displayed_program, colorholder);
        for (i = 0; i < updatedDatasets.length; i++) {
            wtpCostChart.data.datasets[i].data = updatedDatasets[i].data;
        }
        wtpCostChart.options.scales.x.min = -range;
        wtpCostChart.options.scales.x.max = range;
        wtpCostChart.update();
    }
}

function getGraphAssumptions() {
    // The assumption Select Box Ids have to be in the correct order!!
    // The correct order is given by order of the assumptions in the csv / json file names
    let assumptionSelectBoxesIds = ["discountRateAssumption", "taxRateAssumption", "returnsToSchoolingAssumption", "co2externality", "eti"];
    let specifiedAssumptions = [];

    for (let selectbox of assumptionSelectBoxesIds) {
        specifiedAssumptions.push(document.getElementById(selectbox).value);
    }
    return specifiedAssumptions;
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
}

function openTooltipCurrentProgram() {
    if (currently_displayed_program) {
        openTooltip(currently_displayed_program);
    }
}

function openTooltip(program) {
    // Find coordinates of point:
    let coordinates;

    // Assume that the metaset number is given by the position of the programs category in the categories array. By the way the data that is fed to chartJS is generated this should be guaranteed and saves a potentially inefficient linear search.
    let metaset_number = categories.indexOf(unmodified_dataset[program].category);
    let relevant_metaset = currently_displayed_chart.getDatasetMeta(metaset_number);

    for (let j = 0; j < relevant_metaset._dataset.data.length; j++) {
        if (relevant_metaset._dataset.data[j].program === program) {
            // Here we are assuming that the position in _dataset.data matches the position of in data. This seems to be the case!
            coordinates = relevant_metaset.data[j].getCenterPoint();
        }
    }

    // Thx; @jwerre https://stackoverflow.com/questions/39283177/programmatically-open-and-close-chart-js-tooltip
    // Open Tooltip by simulating a mouse press
    let mouseMoveEvent, rectangle;
    rectangle = currently_displayed_chart.canvas.getBoundingClientRect();

    mouseMoveEvent = new MouseEvent('mousemove', {
        clientX: rectangle.left + coordinates.x,
        clientY: rectangle.top + coordinates.y
    });
    currently_displayed_chart.canvas.dispatchEvent(mouseMoveEvent);
}

function addAllPositivesSubtractAllNegatives(array) {
    let negative = 0;
    let positive = 0;

    for (let i = 0; i < array.length; i++) {
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
    let relevant_datapoint = getUnmodifiedProgram(program);

    let max = addAllPositivesSubtractAllNegatives([
        Math.abs(parseFloat(relevant_datapoint["willingness_to_pay"]),
            -Math.abs(parseFloat(relevant_datapoint["government_net_costs"])))
    ]);
    let mappingEntry = getVariableMapping(program);

    barComponents = mappingEntry["willingness_to_pay"];
    let array = [];
    for (let component in barComponents) {
        array.push(parseFloat(relevant_datapoint[component]));
    }
    max = Math.max(addAllPositivesSubtractAllNegatives(array), max);


    barComponents = mappingEntry["government_net_costs"];
    array = [];
    for (let component in barComponents) {
        array.push(parseFloat(relevant_datapoint[component]));
    }
    max = Math.max(addAllPositivesSubtractAllNegatives(array), max);

    return (max);
}

function drawBarChart(variable_to_plot, program, chartElement) {
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
            datasets: generateBarChartProgramData(variable_to_plot, program, colorholder)
        },
        options: {
            indexAxis: 'y',
            responsive: true,
            maintainAspectRatio: false,
            aspectRatio: smallscreen ? 2 : 4,
            //devicePixelRatio: 4, //Set this to save a high res png. Otherwise leave default
            plugins: {
                legend: {
                    display: false,
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
                        },
                        label: function (data) {
                            let confidence_intervall = ""
                            if (unmodified_dataset[currently_displayed_program][data.dataset["effect_key"] + "_95ci_lower"]) {
                                confidence_intervall = `\n95% CI: [${parseFloat(unmodified_dataset[currently_displayed_program][data.dataset["effect_key"] + "_95ci_lower"]).toFixed(2)}€, ${parseFloat(unmodified_dataset[currently_displayed_program][data.dataset["effect_key"] + "_95ci_upper"]).toFixed(2)}€]`
                                return [`${data.dataset.label}: ${data.formattedValue}€`, confidence_intervall]
                            }
                            return [`${data.dataset.label}: ${data.formattedValue}€`];
                        }
                    }
                },
                htmlLegend: {
                    containerID: variable_to_plot + "legend"
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
        },
        plugins: [htmlLegendPlugin]
    });

    // Disable disabled effects:
    if (variable_to_plot === "willingness_to_pay" || variable_to_plot ==="government_net_costs") {
        for (let disabled_effect of disabled_effects[program][variable_to_plot]) {
            for (let dataset of barChart.data.datasets) {
                if (getVariableMapping(program)[variable_to_plot][disabled_effect] == dataset.label) {
                    // hide dataset
                    dataset.hidden = true
                    barChart.update();
                }
            }
        }
    }

    return (barChart);
}

function drawMVPFChart(csv_as_array) {
    // Number Of Programs at Tooltip.
    let tooltip_number = 1;

    let mvpfChartElement = document.getElementById('mvpfChart');
    mvpfChart = new Chart(mvpfChartElement, {
        type: 'scatter',
        data: {
            datasets: generateAllProgramsDatasets(csv_as_array, "scatter")
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
                    display: false,
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
                            let program_identifier = data.dataset.data[data.dataIndex]["program"];
                            let unmodified_datapoint = applyDisableEffects(program_identifier);
                            let tooltip = [];
                            let mvpfIsInfinity = unmodified_datapoint["mvpf"] == "Inf"
    
                            if (tooltip_number > 1) {
                                tooltip.push(program_name + ":");
                            }

                            let confidence_intervall = "";
                            // The second check is to ensure that no effect was disabled
                            if (unmodified_dataset[program_identifier]["mvpf_95ci_lower"] && unmodified_datapoint["mvpf"] == unmodified_dataset[program_identifier]["mvpf"]) {
                                confidence_intervall = `, 95% CI [${mvpfToString(unmodified_dataset[program_identifier]["mvpf_95ci_lower"])}, ${mvpfToString(unmodified_dataset[program_identifier]["mvpf_95ci_upper"])}]`
                            }
    
                            tooltip.push("Willingness to Pay: " + +parseFloat(unmodified_datapoint["willingness_to_pay"]).toFixed(2) + "€");
                            tooltip.push("Government Net Cost: " + +parseFloat(unmodified_datapoint["government_net_costs"]).toFixed(2) + "€");
                            tooltip.push("MVPF: " + (mvpfIsInfinity ? "∞" : +parseFloat(unmodified_datapoint["mvpf"]).toFixed(2)) + confidence_intervall);
    
                            if (tooltip_number > 1 & tooltip_counter < tooltip_number) {
                                tooltip.push("");
                            }
                            tooltip_counter++;
                            return tooltip;
                        }
                    }
                },
                htmlLegend: {
                    containerID: "mvpfChartLegend"
                }
            },
            hover: {
                mode: "point"
            }
        },
        plugins: [htmlLegendPlugin]
    });
    
    currently_displayed_chart = mvpfChart;
    
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

function drawListCIChart(json_as_array) {
    let mvpfChartElement = document.getElementById('mvpfChart');
    // In this chart type the height needs to scale with the number of elements. Otherwise there will be ugly overlap:
    let list_chart_datasets =  generateAllProgramsDatasets(json_as_array, "listci_pe");
    let number_of_elements = list_chart_datasets.reduce((sum, current_dataset) => {
        return sum + current_dataset.data.length;
    }, 0)
    // Now add the confidence interval datasets:
    list_chart_datasets = [...list_chart_datasets, ...generateAllProgramsDatasets(json_as_array, "listci_ci")];

    let mvpfChartDiv = document.getElementById('mvpfChartDiv');
    mvpfChartDiv.style.height = `${number_of_elements * 45}px`

    // We may want two identical x-axis (one on top one on the bottom).
    const x_axis = {
        stacked: true,
        suggestedMin: lower_cutoff,
        suggestedMax: infinity_cutoff + 1,
        position: "top",
        grid: {
            color: (context) => {
                if (context.tick.value === infinity_cutoff + 1) {
                    return 'rgba(0, 0, 0, 1)';
                }
                return "rgba(0, 0, 0, 0.1)";
            },
            display: true,
            drawBorder: false
        },
        ticks: {
            min: lower_cutoff,
            autoSkip: false,
            max: infinity_cutoff,
            stepSize: 1,
            callback: MVPFtickCallback
        }
    }

    const x_axis2 = {
        stacked: true,
        suggestedMin: lower_cutoff,
        suggestedMax: infinity_cutoff + 1,
        position: "bottom",
        grid: {
            display: false,
            drawBorder: false
        },
        ticks: {
            min: lower_cutoff,
            autoSkip: false,
            max: infinity_cutoff,
            stepSize: 1,
            callback: MVPFtickCallback
        }
    }

    mvpfCIChart = new Chart(mvpfChartElement, {
        type: 'line',
        data: {
            datasets: list_chart_datasets
        },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            radius: 10,
            hoverRadius: 12,
            indexAxis: "y",
            scales: {
                x: x_axis,
                x2: x_axis2,
                y: {
                    stacked: true,
                    ticks: {
                        autoSkip: false,
                        callback: function (value, index, values) {
                            let label= this.getLabelForValue(value);
                            // The labels can get a bit too long. Split if longer than 20 characters
                            if (label.length >= 15) {
                                // Find space that is closest to the middle of the string
                                let middle = Math.floor(label.length / 2);
                                let offset = 0;
                                while(middle - offset > 0) {
                                    if (label[middle - offset] === " ") {
                                        return [label.substring(0, middle - offset), label.substring(middle - offset, label.length)]
                                    }
                                    if (label[middle + offset] === " ") {
                                        return [label.substring(0, middle + offset), label.substring(middle + offset, label.length)]
                                    }
                                    offset++;
                                }
                            }
                            return label
                        }
                    },
                    grid: {
                        display: false,
                        drawBorder: false
                    }
                }
            },
            plugins: {
                legend: {
                    display: false,
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
                                return title;
                            }
                            tooltip_number = 1;
                            tooltip_counter = 1;
                            return data[0].dataset.data[data[0].dataIndex]["program_name"];
                        },
                        label: function (data) {
                            // Get the name of the program. Since we have censored the data to draw the graph we need to look up the true values:
                            let program = data.dataset.data[data.dataIndex]["program"];
                            let unmodified_datapoint = applyDisableEffects(program);
                            let tooltip = [];
                            let mvpfIsInfinity = unmodified_datapoint["mvpf"] == "Inf"

                            let confidence_intervall = "";
                            // The second check is to ensure that no effect was disabled
                            if (unmodified_dataset[program]["mvpf_95ci_lower"] && unmodified_datapoint["mvpf"] == unmodified_dataset[program]["mvpf"]) {
                                confidence_intervall = `, 95% CI [${mvpfToString(unmodified_dataset[program]["mvpf_95ci_lower"])}, ${mvpfToString(unmodified_dataset[program]["mvpf_95ci_upper"])}]`
                            }

                            if (tooltip_counter == 1) {
                                tooltip.push("Willingness to Pay: " + +parseFloat(unmodified_datapoint["willingness_to_pay"]).toFixed(2) + "€");
                                tooltip.push("Government Net Cost: " + +parseFloat(unmodified_datapoint["government_net_costs"]).toFixed(2) + "€");
                                tooltip.push("MVPF: " + (mvpfIsInfinity ? "∞" : +parseFloat(unmodified_datapoint["mvpf"]).toFixed(2)) + confidence_intervall);
                            }
                            tooltip_counter++;
                            return tooltip;
                        }
                    }
                },
                htmlLegend: {
                    containerID: "mvpfChartLegend"
                }
            },
            hover: {
                mode: "point"
            }
        },
        plugins: [htmlLegendPlugin]
    });

    currently_displayed_chart = mvpfCIChart;

    mvpfChartElement.onclick = function (evt) {
        //This is totally weird, see https://github.com/chartjs/Chart.js/issues/2292 
        //But it works now!!!!!
        let activePoints = mvpfCIChart.getElementsAtEventForMode(evt, 'point', mvpfCIChart.options);
        let firstPoint = activePoints[0];
        // activePoints is undefined in case no point is pressed. if(undefined) is false, this condition prevents
        // the code from being executed if nothing is clicked.
        if (activePoints[0]) {
            let clicked_program = mvpfCIChart.data.datasets[firstPoint.datasetIndex].data[firstPoint.index].program;
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
        colorholder.color = 1;
        drawBarChart("willingness_to_pay", current_program, singleHTML.chartElements[1]);
        drawBarChart("government_net_costs", current_program, singleHTML.chartElements[0]);
        drawBarChart("mvpf", current_program, singleHTML.chartElements[2]);
        allBarCharts.push(singleHTML.chartElements);
    }
}

function generateLeftSideHTMLCharts(program) {
    generateSingleProgramHTML(program)

    colorholder.color = 1;
    wtpChart = drawBarChart("willingness_to_pay", program, document.querySelector("#wtpgraph"));
    governmentCostChart = drawBarChart("government_net_costs", program, document.querySelector("#costgraph"));
    wtpCostChart = drawBarChart("mvpf", program, document.querySelector("#wtpcostgraph"));
    
    // Increase size of bar Charts if legend has to many elements:
    /*
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
    */
}

function generateSingleProgramHTML(program) {
    program_data = getUnmodifiedProgram(program);
    
    // Generate author citiations
    let links = program_data.links.split(";");
    let authors = program_data.sources.split(";");
    let bibtexkeys = program_data.bibtexkeys.split(";");
    let datasources;
    if (!! program_data.other_data_sources){
        datasources = program_data.other_data_sources.split(";");
    }
    let listElements = "";
    for (let i = 0; i < authors.length; i++) {
        let bibtexelement = literature[bibtexkeys[i]];
        listElements += `<li>${authors[i]}, "${bibtexelement.title}", <i>${bibtexelement.journal.name}</i> <a href="${links[i]}" class="btn linkbutton" target="_blank">LINK</a></li>`
    }

    if (datasources) {
        for (let i = 0; i < datasources.length; i++) {
            let bibtexelement = literature[datasources[i]];
             
            // parse bibtex
            function parseAuthors(authors, full_name) {
                formatted_authors = authors.map(author => {
                    // This splits first and last name of the authors. If there is no first name. E.g because the author is
                    // Destatis, then the first name field is left empty
                    let names = author.name.split(', ');
                    let last_name = names[0];
                    let first_name = names[1];
                    if (first_name == " ") return last_name;
                    return {
                        first_name: first_name,
                        last_name: last_name
                    };
                });
                if (formatted_authors.length === 1) {
                    if (full_name) return formatted_authors[0].first_name + " " + formatted_authors[0].last_name
                    return formatted_authors[0].last_name;
                }
                else if (formatted_authors.length === 2) {
                    return formatted_authors[0].last_name + " & " + formatted_authors[1].last_name
                } 
                else {
                    return formatted_authors[0].last_name + "et al."
                }
            }
            listElements += `<li>${parseAuthors(bibtexelement.author, bibtexelement.fullname == "yes")} (${bibtexelement.year}), "${bibtexelement.title}"
            ${bibtexelement.journal ? `<i>${bibtexelement.journal.name}</i>` : ""} 
            ${bibtexelement.url ? `<a href="${bibtexelement.url}" class="btn linkbutton" target="_blank">LINK</a>` : ""}</li>`

        }
    }

    let headline_tags = "";
    let identification = {
        RCT : ["Randomized Control Trial", "https://en.wikipedia.org/wiki/Randomized_controlled_trial"],
        IV : ["Instrumental Variable Approach", "https://en.wikipedia.org/wiki/Instrumental_variables_estimation"],
        RDD : ["Regression Discontinuity Design", "https://en.wikipedia.org/wiki/Regression_discontinuity_design"],
        DiD : ["Difference in Differences", "https://en.wikipedia.org/wiki/Difference_in_differences"],
        CSR : ["Cross-sectional Regression", "https://en.wikipedia.org/wiki/Cross-sectional_regression"],
        SEM : ["Structural Equilibrium Model", "https://en.wikipedia.org/wiki/Simultaneous_equations_model"],
        PSM : ["Propensity Score Matching", "https://en.wikipedia.org/wiki/Propensity_score_matching"],
        Other : ["Other", "https://mvpfde.de"]
    }
    // Assign to identification "Other" if the value is somehow not defined or not known
    program_data.identification = program_data.identification in identification ? program_data.identification  : "Other"

    headline_tags += `<a href=${identification[program_data.identification][1]} class="btn linkbutton" target="_blank">${identification[program_data.identification][0]}</a>`
    headline_tags += `<a href="https://whatworksgrowth.org/public/files/Scoring-Guide.pdf" style="" class="btn linkbutton" target="_blank">Maryland SMS: ${program_data.maryland_scale}</a>`
    headline_tags += program_data.what_works_included ? `<a href="" style="pointer-events: none;" class="btn linkbutton" target="_blank">Included in STEP</a>` : ""
    headline_tags += program_data.peer_reviewed ? `<a href="" style="pointer-events: none;" class="btn linkbutton" target="_blank">Primary Source Peer Reviewed</a>` : ""
    

    // Generate Description, Bar Chart divs etc..
    rightHandSideDiv = document.querySelector("#rightsidebarcharts");
    let updated_mvpf_related_values = applyDisableEffects(currently_displayed_program)
    let confidence_intervall = "";
    if (unmodified_dataset[currently_displayed_program]["mvpf_95ci_lower"]) {
        confidence_intervall = updated_mvpf_related_values.effect_disabled ? "" : `, 95% CI [${mvpfToString(unmodified_dataset[currently_displayed_program]["mvpf_95ci_lower"])}, ${mvpfToString(unmodified_dataset[currently_displayed_program]["mvpf_95ci_upper"])}]`;
    }
    rightHandSideDiv.innerHTML = `
    <h3>${program_data.program_name}<hr style="margin-bottom: 5px"><small class="text-muted" id="mvpfDisplay" style="font-style: italic;">
    MVPF = ${updated_mvpf_related_values["mvpf"] == "Inf" ? "∞" : updated_mvpf_related_values["mvpf"].toFixed(2)}${confidence_intervall}</small></h3>
    ${headline_tags}

            <button type="button" class="btn collapseicon" data-toggle="collapse"
              data-target="#refdescription">Description</button>
            <div id="refdescription" class="collapse show"><p>${program_data.short_description}</p></div>

            <button type="button" class="btn collapseicon" data-toggle="collapse" data-target="#wtpgraphcontainer">Willingness
              to Pay:</button>
            <div id="wtpgraphcontainer" class="graphcontainer collapse show">
            <div class="barChartDiv" id="wtpgraphdiv">
            <canvas id="wtpgraph"></canvas>
            </div>
            <div id="willingness_to_paylegend"></div>
            </div>

            <button type="button" class="btn collapseicon" data-toggle="collapse" data-target="#costgraphcontainer">Government Net
              Cost:</button>
            <div id="costgraphcontainer" class="graphcontainer collapse show">
            <div class="barChartDiv" id="costgraphdiv">
            <canvas id="costgraph"></canvas>
            </div>
            <div id="government_net_costslegend"></div>
            </div>

            <button type="button" class="btn collapseicon" data-toggle="collapse" data-target="#wtpcostgraphcontainer">Government Net Cost & Willingness to Pay:</button>
            <div id="wtpcostgraphcontainer" class="graphcontainer collapse show">
            <div class="barChartDiv" id="wtpcostgraphdiv">
            <canvas id="wtpcostgraph"></canvas>
            </div>
            <div id="mvpflegend"></div>
            </div>

            <button type="button" class="btn collapseicon" data-toggle="collapse"
              data-target="#relliterature">Relevant Literature & Data Sources:</button>
            <div id="relliterature" class="collapse show">
            <ul>

            </ul>
            ${listElements}
            </div>
      `
}

/*
function generateSingleProgramHTML(program) {
    program_data = getUnmodifiedProgram(program);

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

    for (let key in unmodified_dataset) {
        program = unmodified_dataset[key];
        var option = document.createElement("option");
        option.value = program.program;
        option.innerHTML = program.program_name;
        option.setAttribute("data-subtext",program.category)
        selection.appendChild(option);
    }
    // The select picker needs to be updated
    jQuery('#highlightProgram').selectpicker('refresh');
}

function populateCategories() {
    // adds available categories to the categories select box
    let selection = document.querySelector('#highlightCategory');

    for (let category of categories) {
        var option = document.createElement("option");
        option.value = category;
        option.innerHTML = category;
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
    drawMVPFChart(json_as_array);
    //drawListCIChart(json_as_array);
    populatePrograms();
    populateCategories();
}