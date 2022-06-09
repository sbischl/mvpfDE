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

var shared_assumptions = {};

// Contains a reference to a color index which is passed around and incremented when another color is used in the bar charts.
colorholder = {
    color: 1
}

var links = {
    "taxReform1990": "absenkungen-des-spitzensteuersatzes.html#absenkungen-des-spitzensteuersatzes",
    "taxReform2001": "absenkungen-des-spitzensteuersatzes.html#absenkungen-des-spitzensteuersatzes",
    "taxReform2004": "absenkungen-des-spitzensteuersatzes.html#absenkungen-des-spitzensteuersatzes",
    "taxReform2005": "absenkungen-des-spitzensteuersatzes.html#absenkungen-des-spitzensteuersatzes",
    "qykgbtycul": "andere-arbeitsmarktinterventionen.html#andere-arbeitsmarktinterventionen",
    "jobSearchInformation": "andere-arbeitsmarktinterventionen.html#informationsbrosch%C3%BCre-%C3%BCber-chancen-am-arbeitsmarkt",
    "expectedPensionLetter": "andere-arbeitsmarktinterventionen.html#informationsschreiben-%C3%BCber-zu-erwartende-rentenanspr%C3%BCche",
    "relocationAssistance": "andere-arbeitsmarktinterventionen.html#umzugskosten%C3%BCbernahme",
    "decentralizedEmploymentServices": "andere-arbeitsmarktinterventionen.html#dezentralisierung-von-jobcenter",
    "placementService": "andere-arbeitsmarktinterventionen.html#inhouse-stellenvermittlung",
    "unemploymentBenefits2006": "arbeitslosenversicherung.html#arbeitslosenversicherung",
    "unemploymentBenefits2002": "arbeitslosenversicherung.html#arbeitslosenversicherung",
    "unemploymentBenefits42": "arbeitslosenversicherung.html#arbeitslosenversicherung",
    "unemploymentBenefits44": "arbeitslosenversicherung.html#arbeitslosenversicherung",
    "unemploymentBenefits49": "arbeitslosenversicherung.html#arbeitslosenversicherung",
    "jobCreationSchemes": "bezuschusste-tätigkeiten.html#arbeitsbeschaffungsma%C3%9Fnahmen",
    "oneEuroJobs": "bezuschusste-tätigkeiten.html#ein-euro-jobs",
    "subsidizedJobOpportunities": "bezuschusste-tätigkeiten.html#arbeitsgelegenheiten",
    "negativeIncomeTax": "bezuschusste-tätigkeiten.html#lohnzuschuss",
    "bafoegRepayment": "bildungsreformen.html#baf%C3%B6g-reform-1990",
    "bafoeg2001": "bildungsreformen.html#baf%C3%B6g-reform-2001",
    "interimDegrees": "bildungsreformen.html#realschulabschluss-mit-beendigung-der-10.-klasse-des-gymnasiums",
    "compulsarySchooling": "bildungsreformen.html#verl%C3%A4ngerung-der-hauptschulemittelschule-um-1-jahr",
    "tuitionFees": "bildungsreformen.html#studiengeb%C3%BChren",
    "schoolFees": "bildungsreformen.html#abschaffung-von-schulgeb%C3%BChren",
    "BestUpInformationWorkshop": "bildungsreformen.html#informations-workshop-zum-thema-studium",
    "rockYourLife": "bildungsreformen.html#mentoringprogramm-rock-your-life",
    "mentoringBalu": "bildungsreformen.html#mentoringprogramm-balu-und-du",
    "G8": "bildungsreformen.html#g8-reform",
    "trackingBavaria": "bildungsreformen.html#aufteilung-realschulehauptschule-ab-4.-klasse",
    "bnaiisghtt": "#elternzeitreformen",
    "parentalLeave2007": "elternzeitreformen.html#elterngeld",
    "homeCareSubsidy": "elternzeitreformen.html#betreuungsgeld",
    "maternityLeave79": "elternzeitreformen.html#mutterschaftsurlaubsgeld-1979",
    "maternityLeave86": "elternzeitreformen.html#erziehungsgeld-1986",
    "maternityLeave92": "elternzeitreformen.html#k%C3%BCndigungsschutzausweitung-f%C3%BCr-m%C3%BCtter-1992",
    "eegSolar": "erneuerbare-energien-gesetz.html#f%C3%B6rderung-von-erneuerbaren-energien-im-rahmen-des-erneuerbare-energien-gesetzes",
    "eegWind": "erneuerbare-energien-gesetz.html#f%C3%B6rderung-von-erneuerbaren-energien-im-rahmen-des-erneuerbare-energien-gesetzes",
    "bridgingAllowance": "startup-subventionen.html#%C3%BCberbr%C3%BCckungsgeld",
    "startupSubsidy": "startup-subventionen.html#existenzgr%C3%BCndungszuschuss",
    "startupGrant": "startup-subventionen.html#gr%C3%BCndungszuschuss",
    "trainingMeasures": "weiterbildungs--und-trainingsprogramme.html#trainingsma%C3%9Fnahmen",
    "classRoomTraining": "weiterbildungs--und-trainingsprogramme.html#berufliche-weiterbildung",
    "trainingVoucher": "weiterbildungs--und-trainingsprogramme.html#bildungsgutscheine",
    "shortTraining": "weiterbildungs--und-trainingsprogramme.html#kurze-trainingsprogramme",
    "longTraining": "weiterbildungs--und-trainingsprogramme.html#lange-trainingsprogramme",
    "retraining": "weiterbildungs--und-trainingsprogramme.html#berufliche-neuausrichtung",
    "practiceFirm": "weiterbildungs--und-trainingsprogramme.html#%C3%BCbungsfirma"
}

// Updater to keep track of which charts to update when data changes
class barChartUpdater {
    constructor() {
        this.barChartsToUpdate = []
    }
    registerChart(barChart) {
        this.barChartsToUpdate.push(barChart);
    }
    updateAllCharts() {
        colorholder.color = 1;
        let adjust_prices = false;
        if (barChart.assumption_key && barChart.assumption_key != "master_assumption") {
            adjust_prices = true;
        }
        this.barChartsToUpdate.forEach(barChart => {
            //chart: barChart, barChart.chart
            //program: program,
            //variable_to_plot: variable_to_plot
            let range = getScalesMinMax(barChart.program);
            let updatedDatasets = generateBarChartProgramData(barChart.variable_to_plot, barChart.program, colorholder, adjust_prices);
            for (let i = 0; i < updatedDatasets.length; i++) {
                barChart.chart.data.datasets[i].data = updatedDatasets[i].data;
            }
            barChart.chart.options.scales.x.min = -range;
            barChart.chart.options.scales.x.max = range;
            barChart.chart.update();

            // If we are updating something that has an assumption key that means that there is also a textfield that displays the mvpf of that reform.
            if (barChart.assumption_key) {
                let prices_year = 2010;
                if (barChart.assumption_key != "master_assumption") {
                    prices_year = getUnmodifiedProgram(barChart.program)["prices_year"];
                }
                let insgesamt_text = `Insgesamt: ${barChart.variable_to_plot == "government_net_costs" ? `${parseNumbers(changePriceLevel(from = 2010, to = prices_year) * getUnmodifiedProgram(barChart.program)["government_net_costs"])}` : `${parseNumbers(changePriceLevel(from = 2010, to = prices_year) * getUnmodifiedProgram(barChart.program)["willingness_to_pay"])}`}€ ⇒ MVPF: ${parseNumbers(getUnmodifiedProgram(barChart.program)["mvpf"], true)}`
                console.log(barChart.assumption_key + barChart.variable_to_plot + "gesamttext");
                document.getElementById(barChart.assumption_key + barChart.variable_to_plot + "gesamttext").innerHTML = insgesamt_text;
            }
        });
    }
}
var myChartUpdater = new barChartUpdater();

// This counts the number of categories / or datasets in the chart.js context that have been added so far
var bar_counter = 1;

// HTML Legend plugin for Chart.js (see Chart.js documentation)
const getOrCreateLegendList = (chart, id) => {
    const legendContainer = document.getElementById(id);
    let listContainer = legendContainer.querySelector('ul');

    if (!listContainer) {
        listContainer = document.createElement('ul');
        //listContainer.style.display = 'flex';
        //listContainer.style.flexDirection = 'row';
        //listContainer.style["flex-wrap"] = "wrap";
        listContainer.style.margin = 0;

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
            //li.style.alignItems = 'center';
            li.style.cursor = 'pointer';
            li.style.display = 'inline-block';
            li.style.marginBottom = '0px';
            li.style.marginTop = '0px';

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
            /*

            */
            const boxSpan = document.createElement('span');
            boxSpan.style.background = item.fillStyle;
            boxSpan.style.borderColor = item.strokeStyle;
            boxSpan.style.borderWidth = item.lineWidth + 'px';
            boxSpan.style.marginBottom = "-2px";
            boxSpan.style.display = 'inline-block';
            boxSpan.style.height = '15px';
            boxSpan.style.marginRight = '5px';
            boxSpan.style["border-radius"] = '4px';
            boxSpan.style.width = '30px';

            // Text
            const textContainer = document.createElement('p');
            textContainer.style.color = item.fontColor;
            textContainer.style.margin = 0;
            textContainer.style.marginRight = '10px';
            textContainer.style.padding = 0;
            textContainer.style.fontSize = "16px"
            textContainer.style.display = 'inline';
            textContainer.style.textDecoration = item.hidden ? 'line-through' : '';

            const text = document.createTextNode(item.text);
            textContainer.appendChild(text);

            const inter_med_div = document.createElement('div');
            inter_med_div.appendChild(boxSpan);
            inter_med_div.appendChild(textContainer);

            li.appendChild(inter_med_div);
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

async function updateGraphAssumptions(assumptions = default_assumptions) {
    assumptions_object = assumptions;
    readjson(Object.values(assumptions).join("")).then(function (json_as_array) {
        updateGraphDataSet(json_as_array);
        openTooltipCurrentProgram();
        // We also have to update the MVPF on the right side
        updateProgramHeadLine();
    });
}

async function updateLinkedAssumptions(assumptions, shared_assumption_key) {
    shared_assumptions[shared_assumption_key].assumptions = assumptions;
    readjson(Object.values(assumptions).join("")).then(function (json_as_array) {
        shared_assumptions[shared_assumption_key].updater.updateAllCharts();
        if (shared_assumption_key == "master_assumption") {
            // We need to Update the MVPF Chart:
            updateMVPFDataSetOnly(json_as_array);
            updateProgramHeadLine();
        }
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
    mvpfChartDiv.style.height = `55vh`

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

    mvpfChart.destroy();
    drawListCIChart(json_as_array_copy);
}

function updateProgramHeadLine(program = currently_displayed_program) {
    // this function manipulates the HTML of the Bar Charts
    let updated_mvpf_related_values = applyDisableEffects(program)
    // We cannot display the confidence interval if some of the effects were disabled.
    let confidence_intervall = "";
    if (unmodified_dataset[program]["mvpf_95ci_lower"]) {
        confidence_intervall = updated_mvpf_related_values.effect_disabled ? "" : `, 95% CI [${mvpfToString(unmodified_dataset[program]["mvpf_95ci_lower"])}, ${mvpfToString(unmodified_dataset[program]["mvpf_95ci_upper"])}]`;
    }
    let mvpf_to_print = mvpfToString(updated_mvpf_related_values.mvpf);
    document.querySelector("#mvpfDisplay").innerHTML = `MVPF = ${mvpf_to_print}${confidence_intervall}`;
    loadMVPFText(program);
    loadWTPText(program);
    loadCostText(program);
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

function MVPFtickCallback(value, index, values) {
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

function updateMVPFDataSetOnly(csv_as_array) {
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
    myChartUpdater.updateAllCharts();

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
        updatedDatasets = generateBarChartProgramData("willingness_to_pay", currently_displayed_program, colorholder);
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

function getScalesMinMax(program, adjust_prices = false) {
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
    if (adjust_prices) max *= changePriceLevel(from = 2010, to = relevant_datapoint["prices_year"]);
    return (+max.toFixed(2));
}

function drawBarChart(variable_to_plot, program, chartDiv, shared_assumption_key) {
    currently_displayed_program = program;
    let div = document.getElementById(chartDiv);
    // This creates an assumption button if shared_assumption_key is specified
    let assumption_button = "";
    let adjust_prices = false;
    if (shared_assumption_key) {
        if (shared_assumption_key != "master_assumption") {
            adjust_prices = true;
        }
        if (shared_assumptions[shared_assumption_key]) {
            // We already have a shared assumption, need to add this one;
        }
        else {
            // Need to create a new shared assumption.
            let modal = `<div class="modal fade" id="${shared_assumption_key}" data-keyboard="false" tabindex="-1" role="dialog" aria-hidden="true">
            <div class="modal-dialog" role="document">
              <div class="modal-content">
                <div class="modal-header">
                  <h4 class="modal-title">Annahmen</h4>
                  <button type="button" class="close" onclick="set_assumptions('${shared_assumption_key}', shared_assumptions['${shared_assumption_key}'].assumptions)" data-dismiss="modal" aria-label="Close">
                    <span aria-hidden="true">&times;</span>
                  </button>
                </div>
                <div class="modal-body">
                  <div class="assumption taxRateAssumption">
                    <form>
                      <span class="assumptionText">Steuersatz:</span>
                      <div class="form-check">
                        <input class="form-check-input" name='taxRateAssumption' type="radio"  id="taxa1" value="tax_rate0">
                        <label class="form-check-label" for="taxa1">0%</label>
                      </div>
                      <div class="form-check">
                        <input class="form-check-input" name='taxRateAssumption' type="radio"  id="taxa2" value="tax_rate10">
                        <label class="form-check-label" for="taxa2">10%</label>
                      </div>
                      <div class="form-check">
                        <input class="form-check-input" name='taxRateAssumption' type="radio"  id="taxa3" value="tax_rate30">
                        <label class="form-check-label" for="taxa3">30%</label>
                      </div>
                      <div class="form-check">
                        <input class="form-check-input" name='taxRateAssumption' type="radio"  id="taxa4" value="tax_rate50">
                        <label class="form-check-label" for="taxa4">50%</label>
                      </div>
                      <div class="form-check">
                        <input class="form-check-input" name='taxRateAssumption' type="radio"  id="taxa5"
                          value="tax_ratenonlinear" checked>
                        <label class="form-check-label" for="taxa5">Gesamtes Steuer und Transfersystem</label>
                      </div>
                      <div class="form-check">
                        <input class="form-check-input" name='taxRateAssumption' type="radio"  id="taxa6"
                          value="tax_rateincometaxonly">
                        <label class="form-check-label" for="taxa6">Einkommenssteuer</label>
                      </div>
                    </form>
                  </div>
                  <div class="assumption discountRateAssumption">
                    <form>
                      <span class="assumptionText">Diskontrate:</span>
                      <div class="form-check form-check-inline">
                        <input class="form-check-input" name="discountRateAssumption" type="radio"  id="disca1" value="discount_rate1">
                        <label class="form-check-label" for="disca1">1%</label>
                      </div>
                      <div class="form-check form-check-inline">
                        <input class="form-check-input" name="discountRateAssumption" type="radio"  id="disca2" value="discount_rate3"
                          checked>
                        <label class="form-check-label" for="disca2">3%</label>
                      </div>
                      <div class="form-check form-check-inline">
                        <input class="form-check-input" name="discountRateAssumption" type="radio"  id="disca3" value="discount_rate7">
                        <label class="form-check-label" for="disca3">7%</label>
                      </div>
                    </form>
                  </div>
                  <div class="assumption returnsToSchoolingAssumption">
                    <form>
                      <span class="assumptionText">Bildungsrendite:</span>
                      <div class="form-check form-check-inline">
                        <input class="form-check-input" name= "returnsToSchoolingAssumption" type="radio"  id="schoola1"
                          value="returns_to_schooling5">
                        <label class="form-check-label" for="schoola1">5%</label>
                      </div>
                      <div class="form-check form-check-inline">
                        <input class="form-check-input" name= "returnsToSchoolingAssumption" type="radio"  id="schoola2"
                          value="returns_to_schooling7">
                        <label class="form-check-label" for="schoola2">7%</label>
                      </div>
                      <div class="form-check form-check-inline">
                        <input class="form-check-input" name= "returnsToSchoolingAssumption" type="radio"  id="schoola3"
                          value="returns_to_schooling11">
                        <label class="form-check-label" for="schoola3">11%</label>
                      </div>
                      <div class="form-check form-check-inline">
                        <input class="form-check-input" name= "returnsToSchoolingAssumption" type="radio"  id="schoola4"
                          value="returns_to_schoolingIAB" checked>
                        <label class="form-check-label" for="schoola4">IAB Daten</label>
                      </div>
                    </form>
                  </div>
          
                  <div class="assumption co2externality">
                    <form>
                      <span class="assumptionText">C02 Preis:</span>
                      <div class="form-check form-check-inline">
                        <input class="form-check-input" name= "co2externality" type="radio"  id="co2a1"
                          value="co2_externality0">
                        <label class="form-check-label" for="co2a1">0€</label>
                      </div>
                      <div class="form-check form-check-inline">
                        <input class="form-check-input" name= "co2externality" type="radio"  id="co2a2"
                          value="co2_externality100" checked>
                        <label class="form-check-label" for="co2a2">100€</label>
                      </div>
                      <div class="form-check form-check-inline">
                        <input class="form-check-input" name= "co2externality" type="radio"  id="co2a3"
                          value="co2_externality250">
                        <label class="form-check-label" for="co2a3">200€</label>
                      </div>
                    </form>
                  </div>
          
                  <div class="assumption eti">
                    <form>
                      <span class="assumptionText">Elastizität von versteuerbarem Einkommen:</span>
                      <div class="form-check">
                        <input class="form-check-input" name="eti" type="radio"  id="etia1" value="eti0">
                        <label class="form-check-label" for="etia1">ɛ = 0</label>
                      </div>
                      <div class="form-check">
                        <input class="form-check-input" name="eti" type="radio"  id="etia2" value="etibase" checked>
                        <label class="form-check-label" for="etia2">ɛ = 0.3</label>
                      </div>
                      <div class="form-check">
                        <input class="form-check-input" name="eti" type="radio"  id="etia3" value="eti6">
                        <label class="form-check-label" for="etia3">ɛ = 0.6</label>
                      </div>
                    </form>
                  </div>
                </div>
                <div class="modal-footer">
                  <button type="button" onclick="set_assumptions('${shared_assumption_key}', 'default')" class="btn btn-secondary">Zurücksetzen</button>
                  <button type="button" data-dismiss="modal" onclick="updateLinkedAssumptions(read_assumptions('${shared_assumption_key}'), '${shared_assumption_key}')" class="btn btn-primary">Annahmen übernehmen</button>
                </div>
              </div>
            </div>
          </div>`
            modaldiv = document.createElement('div');
            modaldiv.innerHTML = modal;
            document.body.appendChild(modaldiv)
            shared_assumptions[shared_assumption_key] = {
                updater: new barChartUpdater(),
                assumptions: default_assumptions,
                colorholder: {
                    color: 1
                }
            }
        }
        let prices_year = getUnmodifiedProgram(program)["prices_year"];
        if (shared_assumption_key == "master_assumption") {
            prices_year = 2010;
        }
        let insgesamt_text = `Insgesamt: ${variable_to_plot == "government_net_costs" ? `${parseNumbers(changePriceLevel(from = 2010, to = prices_year) * getUnmodifiedProgram(program)["government_net_costs"])}` : `${parseNumbers(changePriceLevel(from = 2010, to = prices_year) * getUnmodifiedProgram(program)["willingness_to_pay"])}`}€ ⇒ MVPF: ${parseNumbers(getUnmodifiedProgram(program)["mvpf"], true)}`
        assumption_button = `<div class="buttonbox"><span class="chartSubtitle" id="${shared_assumption_key + variable_to_plot + "gesamttext"}">${insgesamt_text}</span><button type="button" class="btn btn-primary assumptionbutton" data-toggle="modal" data-target="#${shared_assumption_key}">Annahmen</button></div>`
    }

    div.innerHTML = `${assumption_button}<div class = "barChartDiv"><canvas id = "${chartDiv}canvas"></canvas></div><div class = "barChartLegend" id = "${chartDiv}legend"></div>`;
    // Set Font size
    Chart.defaults.font.size = 15.5;
    Chart.defaults.font.family = 'Open Sans';

    let local_color_holder = colorholder;
    if (shared_assumption_key) {
        local_color_holder = shared_assumptions[shared_assumption_key].colorholder;
    }

    // Get Plotting range
    let range = getScalesMinMax(program, adjust_prices);


    // Check if the screen size is small
    //let smallscreen = jQuery(window).width() < 1450 ? true : false
    barChart = new Chart(document.getElementById(chartDiv + "canvas"), {
        type: 'bar',
        data: {
            labels: '.', // In ChartJS Beta 3.0 this could be left empty. If we leave it empty in 3.3, there is no graph drawn anymore. (And the tooltip is now shared for all areas of the chart in 3.3). 
            datasets: generateBarChartProgramData(variable_to_plot, program, local_color_holder, adjust_prices)
        },
        options: {
            indexAxis: 'y',
            responsive: true,
            maintainAspectRatio: false,
            //aspectRatio: smallscreen ? 2 : 4,
            //devicePixelRatio: 4, //Set this to save a high res png. Otherwise leave default
            plugins: {
                legend: {
                    display: false,
                    position: 'bottom',
                    usePointStyle: true,
                    onClick: () => { } //This disables the ability to click on the legend and remove effects.
                },
                tooltip: {
                    enabled: true,
                    displayColors: false,
                    bodyColor: "black",
                    backgroundColor: "white",
                    borderWidth: 2,
                    cornerRadius: 8,
                    bodyFont: { weight: 600 },
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
                    containerID: chartDiv + "legend"
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
                    afterTickToLabelConversion: function (data) {
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

    if (shared_assumption_key) {
        shared_assumptions[shared_assumption_key].updater.registerChart({
            chart: barChart,
            program: program,
            variable_to_plot: variable_to_plot,
            assumption_key: shared_assumption_key
        });
    }
    else {
        // Register for updating:
        myChartUpdater.registerChart({
            chart: barChart,
            program: program,
            variable_to_plot: variable_to_plot
        });
    }

    // Disable disabled effects:
    if (variable_to_plot === "willingness_to_pay" || variable_to_plot === "government_net_costs") {
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
            scales: getScales("mvpf", "Jahr", "Marginal Value of Public Funds"),
            plugins: {
                legend: {
                    display: false,
                    position: 'bottom',
                    usePointStyle: true,
                    labels: {
                        filter: function (legendItem, chartData) {
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

                            tooltip.push(`${strings.wtp}: ` + +parseFloat(unmodified_datapoint["willingness_to_pay"]).toFixed(2) + "€");
                            tooltip.push(`${strings.cost}: ` + +parseFloat(unmodified_datapoint["government_net_costs"]).toFixed(2) + "€");
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
            highlightProgram(clicked_program);
        }
    };
}

function drawListCIChart(json_as_array) {
    let mvpfChartElement = document.getElementById('mvpfChart');
    // In this chart type the height needs to scale with the number of elements. Otherwise there will be ugly overlap:
    let list_chart_datasets = generateAllProgramsDatasets(json_as_array, "listci_pe");
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
        title: {
            display: true,
            text: "Marginal Value of Public Funds"
        },
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
        title: {
            display: true,
            text: "Marginal Value of Public Funds"
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
                            let label = this.getLabelForValue(value);
                            // The labels can get a bit too long. Split if longer than 20 characters
                            if (label.length >= 15) {
                                // Find space that is closest to the middle of the string
                                let middle = Math.floor(label.length / 2);
                                let offset = 0;
                                while (middle - offset > 0) {
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
                                tooltip.push(`${strings.wtp}: ` + +parseFloat(unmodified_datapoint["willingness_to_pay"]).toFixed(2) + "€");
                                tooltip.push(`${strings.cost}: ` + +parseFloat(unmodified_datapoint["government_net_costs"]).toFixed(2) + "€");
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
    if (!!program_data.other_data_sources) {
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
        RCT: ["Randomized Control Trial", "https://en.wikipedia.org/wiki/Randomized_controlled_trial"],
        IV: ["Instrumental Variable Approach", "https://en.wikipedia.org/wiki/Instrumental_variables_estimation"],
        RDD: ["Regression Discontinuity Design", "https://en.wikipedia.org/wiki/Regression_discontinuity_design"],
        DiD: ["Difference in Differences", "https://en.wikipedia.org/wiki/Difference_in_differences"],
        CSR: ["Cross-sectional Regression", "https://en.wikipedia.org/wiki/Cross-sectional_regression"],
        SEM: ["Structural Equilibrium Model", "https://en.wikipedia.org/wiki/Simultaneous_equations_model"],
        PSM: ["Propensity Score Matching", "https://en.wikipedia.org/wiki/Propensity_score_matching"],
        Other: ["Other", "https://mvpfde.de"]
    }
    // Assign to identification "Other" if the value is somehow not defined or not known
    program_data.identification = program_data.identification in identification ? program_data.identification : "Other"

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
        console.log(disabled_programs.includes(program.program));
        console.log(program.program);
        if (disabled_programs.includes(program.program)) continue;
        var option = document.createElement("option");
        option.value = program.program;
        option.innerHTML = program.program_name;
        option.setAttribute("data-subtext", program.category)
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
        option.setAttribute("selected", "")
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

// New functions go here:
function set_assumptions(assumption_id, assumptions_to_set = assumptions_object) {
    if (assumptions_to_set === "default") {
        assumptions_to_set = default_assumptions
    }
    let assumption_box = document.getElementById(assumption_id);
    let all_assumptions = [...assumption_box.querySelectorAll('.assumption')];
    for (let assumption of all_assumptions) {
        let assumptionclass = assumption.classList[1];
        let radios = [...assumption.querySelectorAll('.form-check-input')];
        for (let radio of radios) {
            if (assumptions_to_set[assumptionclass] == radio.value) {
                radio.checked = true;
            }
        }
    }
}


function read_assumptions(assumption_id) {
    let assumptionSelectBoxesIds = {
        discountRateAssumption: "",
        taxRateAssumption: "",
        returnsToSchoolingAssumption: "",
        co2externality: "",
        eti: ""
    };
    Object.keys(assumptionSelectBoxesIds).forEach(key => {
        [...document.getElementById(assumption_id).querySelectorAll(`input[name="${key}"]`)].forEach(radio => {
            if (radio.checked) {
                assumptionSelectBoxesIds[key] = radio.value;
            }
        })
    });
    return assumptionSelectBoxesIds;
}

function loadDescription(program) {
    document.getElementById("description").innerHTML = getUnmodifiedProgram(program)["short_description"] + ` Weitere Details zu diesem Regierungsprogramm als auch eine detaillierte Beschreibung der Berechnung des MVPFs sind <a href=${links[program]}>hier</a> zu finden.`;
}

function loadReformName(program) {
    document.getElementById("reformname").innerHTML = getUnmodifiedProgram(program)["program_name"];
}

function loadProgram(program) {
    loadDescription(program);
    loadReformName(program);
    updateProgramHeadLine(program);
    shared_assumptions["master_assumption"].colorholder.color = 1;
    drawBarChart("willingness_to_pay", program,"barChart", "master_assumption");
    drawBarChart("government_net_costs", program,"barChart2", "master_assumption");
}

function intToString(integ) {
    intsAndStrings = {
        1: "einer",
        2: "zwei",
        3: "drei",
        4: "vier",
        5: "fünf",
        6: "sechs",
        7: "sieben",
        8: "acht",
        9: "neun",
        10: "zehn",
        11: "elf",
        12: "zwölf"
    }
    if (integ <= 12) {
        return intsAndStrings[integ]
    }
    return integ
}

function loadCostText(program) {
    let unmodifed_program = getUnmodifiedProgram(program)

    let number_elements = Object.keys(getVariableMapping(program)["government_net_costs"]).length
    let number_string = intToString(number_elements);
    let morethanone = number_elements != 1
    let text =`Die fiskalischen Kosten bestehen aus ${morethanone ? `${number_string} Komponenten` : `lediglich einer Komponente`}. ${morethanone ? "In Summe betragen sie" : "Sie betragen"} ${parseNumbers(unmodifed_program.government_net_costs)}€. ${morethanone ? "Die folgende Grafik zeigt die genaue Zusammensetzung an." : ""}`
    document.getElementById("costtext").innerHTML = text;
}


function loadWTPText(program) {
    let unmodifed_program = getUnmodifiedProgram(program)

    let number_elements = Object.keys(getVariableMapping(program)["willingness_to_pay"]).length
    let number_string = intToString(number_elements);
    let morethanone = number_elements != 1
    let text =`Die Zahlungsbereitschaft setzt sich aus ${morethanone ? `${number_string} Komponenten` : `lediglich einer Komponente`} zusammen. ${morethanone ? "In Summe beträgt sie" : "Sie beträgt"} ${parseNumbers(unmodifed_program.willingness_to_pay)}€. ${morethanone ? "Die folgende Grafik zeigt die Größe der einzelnen Effekte." : ""}`
    document.getElementById("wtptext").innerHTML = text;
}

function loadMVPFText(program) {
    let unmodifed_program = getUnmodifiedProgram(program)
    let text;
    if (unmodifed_program.willingness_to_pay > 0 && unmodifed_program.government_net_costs > 0) {
        text = `Sowohl die Zahlungsbereitschaft als auch die fiskalischen Kosten nach Berücksichtigung der Externalitäten sind positiv. Der MVPF ist somit gegeben durch das Verhältnis der Zahlungsbereitschaft zu den fiskalischen Kosten: MVPF = ${parseNumbers(unmodifed_program.willingness_to_pay)}€ / ${parseNumbers(unmodifed_program.government_net_costs )}€ = ${parseNumbers(unmodifed_program.mvpf, true)}`
    }
    else if (unmodifed_program.willingness_to_pay < 0 && unmodifed_program.government_net_costs > 0) {
        text = `Das Regierungsprogramm weist eine negative Zahlungsbereitschaft bei positiven Kosten auf. Sowohl der Staat als auch die vorgesehenen Nutznießer des Regierungsprogramm wurden schlechter gestellt. Der MVPF ist folglich negativ und entspricht: MVPF = ${parseNumbers(unmodifed_program.willingness_to_pay)}€ / ${parseNumbers(unmodifed_program.government_net_costs)}€ = ${parseNumbers(unmodifed_program.mvpf, true)}`
    }
    else if (unmodifed_program.willingness_to_pay > 0 && unmodifed_program.government_net_costs < 0) {
        text = `Die Zahlungsbereitschaft ist positiv bei gleichzeitig negativen Nettokosten. Der Effekt auf das öffentlich Budget ist dementsprechnd positiv. Der MVPF ist unendlich.`
    }
    else if (unmodifed_program.willingness_to_pay < 0 && unmodifed_program.government_net_costs < 0) {
        text = `Sowohl die Zahlungsbereitschaft als auch die Nettokosten des Regierungsprogramms sind negativ. In diesem kehrt sich die Interpretation des MVPFs um. Er gibt nun an wieviel Nutzen gemessen in Zahlungsbereitschaft verloren geht um einen Euro Steueraufkommen zu generieren. Umgekehrt könnte durch geringere Investition in das Regierungsprogramm pro Euro ein Nutzen in Höhe von MVPF = ${parseNumbers(unmodifed_program.willingness_to_pay)}€ / ${parseNumbers(unmodifed_program.government_net_costs)}€ = ${parseNumbers(unmodifed_program.mvpf, true)} erzeugt werden.`
    }
    else if (unmodifed_program.willingness_to_pay > 0 && unmodifed_program.government_net_costs == 0) {
        text = `Die Zahlungsbereitschaft ist positiv. Zudem enstehen keine Kosten auf Seite des Staates. Somit wird pro eingesetzem Euro eine unendliche Menge an Nutzen, gemessen in Zahlungsbereitschaft, generiert. `
    }
    else if (unmodifed_program.willingness_to_pay == 0 && unmodifed_program.government_net_costs == 0) {
        text = `Sowohl die Zahlungsbereitschaft als auch die Nettokosten sind gleich null. Diese Reform hat gar keinen Effekt. Der MVPF ist daher null.`
    }
    else if (unmodifed_program.willingness_to_pay < 0 && unmodifed_program.government_net_costs == 0) {
        text = `Eine negative Zahlungsbereitschaft bei gleichzeitigen Kosten in Höhe von null sind gleichbedeutend mit einem MVPF von minus unendlich.`
    }
    else {
        text = `Die Zahlungsbereitschaft ist null. Daher ist auch der MVPF null.`
    }
    document.getElementById("mvpftext").innerHTML = text;
}

function highlightProgram(program) {
    loadProgram(program);
    // Also jump to program:
    let reform_heading_element = document.getElementById("reformname")
    if (!elementInViewport(reform_heading_element)) {
        reform_heading_element.scrollIntoView({ behavior: 'smooth' });
    }
    else {
        openTooltipCurrentProgram();
    } 
}

function parseNumbers(number, ismvpf = false, decimals = 2) {
    if (ismvpf) {
        if (number == "Inf") {
            return "∞";
        }
    }
    number_string = number.toFixed(decimals);
    number_string = number_string.replace('.', ',');
    return number_string
}

function jumpInDocument(location) {
    if (location = 'top') {
        document.getElementById("überblick-über-alle-reformen").scrollIntoView({behavior: 'smooth'});
    }
}

function elementInViewport(el) {
    //thx @https://stackoverflow.com/questions/8229291/how-to-check-if-an-element-is-in-the-view-of-the-user-with-jquery
    var top = el.offsetTop;
    var left = el.offsetLeft;
    var width = el.offsetWidth;
    var height = el.offsetHeight;
  
    while(el.offsetParent) {
      el = el.offsetParent;
      top += el.offsetTop;
      left += el.offsetLeft;
    }
  
    return (
      top >= window.pageYOffset &&
      left >= window.pageXOffset &&
      (top + height) <= (window.pageYOffset + window.innerHeight) &&
      (left + width) <= (window.pageXOffset + window.innerWidth)
    );
}

function sleep(seconds) {
    return new Promise((resolve) => {
        setTimeout(resolve, seconds * 1000);
        console.log("spelt");
    });
}
