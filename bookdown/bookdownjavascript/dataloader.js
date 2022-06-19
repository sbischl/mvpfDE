const document_root = '';
const infinity_cutoff = 6;
const lower_cutoff = -1;

const cost_lower_cutoff = -1
const cost_upper_cutoff = 3 
const wtp_lower_cutoff = -1
const wtp_upper_cutoff = 3

// Categories (in order that they are displayed in the legend). These are loaded from data/categories.json:
var categories;

// Default Spec. If this specification is loaded readjson will instead load default.json. This may be desirable to avoid different results (e.g. because
// different number of bootsstrap reps for default & other specs even though the currently selected spec is the default)
const default_spec = "discount_rate3tax_ratenonlinearreturns_to_schoolingIABco2_externality100etibase";

// Default assumptions
const default_assumptions = { // these are the default assumptions
    discountRateAssumption: "discount_rate3",
    taxRateAssumption: "tax_ratenonlinear",
    returnsToSchoolingAssumption: "returns_to_schoolingIAB",
    co2externality: "co2_externality100",
    eti: "etibase"
  }

const disabled_programs = ["speedLimitA3", "speedLimitA61", "coronavirusRestrictions", "sportsExpenditure", "bicycleHelmet"];

const german_mode = true;

// Localization:
const categories_german = { 
    "Top Tax Reform": "Absenkungen des Spitzensteuersatzes",
    "Education": "Bildungsreformen",
    "Job Training": "Weiterbildungs- und Trainingsprogramme",
    "Start-Up Subsidy": "Startup Subventionen",
    "Subsidized Employment": "Bezuschusste Tätigkeiten",
    "Other Labor Market Policies": "Andere Arbeitsmarktinterventionen",
    "Unemployment Insurance": "Arbeitslosenversicherung",
    "Parental Leave Reform": "Elternzeitreformen",
    "Climate Policy": "EEG",
    "Health Program": "Gesundheitspolitik",
    "Other": "Andere"
}
// Change these between German and English
const strings = {
    wtp : "Zahlungsbereitschaft",
    cost: "Fiskalische Kosten"
}

var assumptions_object = default_assumptions;

// Store the additional parameters of each policy. These are loaded from data/programs.json:
var additional_program_info

// Store the mapping of willingness to pay and government net cost in a JSON object. (loaded from data/variable_mapping)
var variable_mapping;

// Store the colors used for the Graph (loaded from data/colors.json);
var colors;

// Store additional information about the literature in an object;
var literature;

// Store a unmodified, easy to access version of all programs (Object):
var unmodified_dataset;

// This will be the array carrying the data. It should be compatible to the array returned from readcsv with the only difference that the json format omits all the keys with NA values.
// A array of this style is passed to various function. json_as_array may be modified (e.g. mvpfs larger than infinity cutoff will be censored to be 6 and 7 indicates infinity)
// To get a json_as_array_copy stores a unmodifed copy.
var json_as_array;

// Store unmodified copy of json as array. This is similar to unmodified_dataset but not an array instead of an object. If the data of a specific program
// needs to be looked up, unmodified dataset should be used because no search is required here. When initially writing all the function performance wasn't
// really a concern ^_^. Most or all instances where an ineffcient linear search was performed on the data have been replaced with time constant lookups in
// unmodified_dataset, but some functions still require that the data that is getting passed is an iterable array. With some refactoring
// I could probably get rid of json_as_array_copy but it is only a minor memory waste, and has the advantage of having an easier iterable list. Keep for now!
var json_as_array_copy;

// Object that keeps track of disabled effects
var disabled_effects = {};

const cpi = {
    1962: 24.783,
    1963: 25.534,
    1964: 26.1348,
    1965: 26.9609,
    1966: 27.8621,
    1967: 28.3878,
    1968: 28.8384,
    1969: 29.3641,
    1970: 30.4155,
    1971: 31.9926,
    1972: 33.7199,
    1973: 36.1231,
    1974: 38.6014,
    1975: 40.9295,
    1976: 42.6568,
    1977: 44.2339,
    1978: 45.4355,
    1979: 47.313,
    1980: 49.8664,
    1981: 53.0206,
    1982: 55.7993,
    1983: 57.6017,
    1984: 59.0286,
    1985: 60.2302,
    1986: 60.1551,
    1987: 60.3053,
    1988: 61.0563,
    1989: 62.7836,
    1990: 64.4358,
    1991: 65.5,
    1992: 68.8,
    1993: 71.9,
    1994: 73.8,
    1995: 75.1,
    1996: 76.1,
    1997: 77.6,
    1998: 78.3,
    1999: 78.8,
    2000: 79.9,
    2001: 81.5,
    2002: 82.6,
    2003: 83.5,
    2004: 84.9,
    2005: 86.2,
    2006: 87.6,
    2007: 89.6,
    2008: 91.9,
    2009: 92.2,
    2010: 93.2,
    2011: 95.2,
    2012: 97.1,
    2013: 98.5,
    2014: 99.5,
    2015: 100,
    2016: 100.5,
    2017: 102,
    2018: 103.8,
    2019: 105.3
}

// Functions
// Simple helper function which loads a url to a json file into a js object
async function loadJSON(url) {
    let json;
    await fetch(url).then(response => {
        json = response.json();
    });
    return json;
}

async function readjson(assumption_string) {
    console.log("Loading data with assumption string: " + assumption_string);
    // depends on additional_program_info being loaded as we need some way of determining which government policies exist before this function can be called
    // First we need to get a list of all programs. Where the entries are strings
    var programs = additional_program_info.map(program => {
        return program.program_identifier;
    });

    if (assumption_string === default_spec) {
        assumption_string = "default";
    }

    // Individual json requests to all of the programs
    var requests = programs.map(program => {
        // Note: usually one would expect that this code runs the loadJSON and stores the
        // result in requests. This is not what we want, as the http requests should be
        // run in parallel for better performance. Fortunately, this is not what is happening. loadJSON is a async function. And async functions return promises, which are not automatically resolved.
        return loadJSON(document_root + "data/" + program + "/" + assumption_string + ".json");
    });

    // Resolve promises in parallel
    await Promise.all(requests).then((result) => json_as_array = result);

    // This loop merges the loaded data with the details about each reform (description, sources, age etc..) form programs.json. The resulting array of objects then contains all relevant data.

    for (var i = 0; i < json_as_array.length; i++) {
        Object.assign(json_as_array[i], additional_program_info[i]);
        // Rename program identifier to program. (For compatibility with the old solution)
        if (german_mode) {
            json_as_array[i]["program_name"] = json_as_array[i]["program_name_de"];
        }
        json_as_array[i]["program"] = json_as_array[i]["program_identifier"];
        delete json_as_array[i]["program_identifier"];
    }

    unmodified_dataset = {};
    for (program of json_as_array) {
        unmodified_dataset[program["program"]] = JSON.parse(JSON.stringify(program));
    }
    console.log("finished loading data");
    return json_as_array;
}

function generateAllProgramsDatasets(json_as_array, dataset_type = "scatter") {
    // This stores the name or in my context the type of program (i.e. something like education policy, tax reform ...) of all the datasets we need to construct
    // This function can be used to generate datasets for different types of charts. This mainly changes additional chart.js config that needs to be included in the dataset
    let datasets_labels = [];
    // The actual dataSets that will eventually be returned
    let datasets = [];
    let i;

    // Sort by category. This allows controlling the order in which the categories are drawn in the legend. And also yields performance benefits because it makes the datasets array that is searched as small as possible.
    json_as_array.sort(function(a, b) {
        return categories.indexOf(a.category) - categories.indexOf(b.category);
    });

    for (i = 0; i < json_as_array.length; i++) {

        // Generate a copy of the observation to prevent censoring or modification of the original data.
        let current_observation = JSON.parse(JSON.stringify(json_as_array[i]));

        // Throw out reforms that are not included
        if (disabled_programs.includes(current_observation.program)) continue;

        // Update values if necessary:
        let updated_values = applyDisableEffects(current_observation.program);
        current_observation.mvpf = updated_values.mvpf;
        current_observation.willingness_to_pay = updated_values.willingness_to_pay;
        current_observation.government_net_costs = updated_values.government_net_costs;
        current_observation.willingness_to_pay_per_program_cost = updated_values.willingness_to_pay_per_program_cost;
        current_observation.government_net_costs_per_program_cost = updated_values.government_net_costs_per_program_cost;

        // The data for the List chart with confidence intervals needs to have a ci key which contains an array with two entries. 0: lower bound of CI and 1: upper bound of CI
        if (dataset_type === "listci_ci") {
            current_observation["mvpf_ci"] = [current_observation["mvpf_95ci_lower"], current_observation["mvpf_95ci_upper"]];
        }

        // Check if the dataset already exists. If not add it.
        if (!datasets_labels.includes(current_observation.category)) {
            datasets_labels.push(current_observation.category);
            if (dataset_type === "scatter") {
                datasets.push(generateEmptyDatasetScatter(current_observation.category, datasets.length + 1));
            }
            else if (dataset_type === "listci_pe") {
                datasets.push(generateEmptyDatasetListCIPE(current_observation.category, datasets.length + 1));
            }
            else if(dataset_type === "listci_ci") {
                datasets.push(generateEmptyDatasetListCICI(current_observation.category, datasets.length + 1));
            }
        }
        // Add current observation to the correct dataset:

        // First get the relevant dataset.
        correctDataset = datasets.find(dataset => {
            return dataset.label === current_observation.category;
        });

        // Now add the observation
        correctDataset.data.push(current_observation);
    }

    // Now the data has been read. Apply the censoring. That means convert infinite values to some number (e.g.) which will act as infinity on the chart.
    censorValues(datasets);

    if (german_mode) {
        datasets = datasets.map(dataset => {
            let dataset_new = dataset
            dataset_new.label = categories_german[dataset.label]
            return dataset_new;
        });
    }

    return (datasets);
}

function initDisabledEffects(json_as_array) {
    for (program of json_as_array) {
        disabled_effects[program["program"]] = {
            "government_net_costs" : [],
            "willingness_to_pay" : []
        };
    }
}

function applyDisableEffects(program) {
    // Apply effect of disabling certain effects:
        // First check if something needs to be changed:
        let effect_disabled = false;

        // Initialize the value that may need updating with their original values.
        let mvpf = unmodified_dataset[program].mvpf;
        
        let government_net_costs_per_program_cost = unmodified_dataset[program].government_net_costs_per_program_cost
        let willingness_to_pay_per_program_cost = unmodified_dataset[program].willingness_to_pay_per_program_cost
        let government_net_costs = unmodified_dataset[program].government_net_costs
        let willingness_to_pay = unmodified_dataset[program].willingness_to_pay

        if (disabled_effects[program]["government_net_costs"].length > 0) {
            for (disabled_effect of disabled_effects[program]["government_net_costs"]) {
                government_net_costs -= unmodified_dataset[program][disabled_effect];
                effect_disabled = true;
            }
            government_net_costs_per_program_cost = government_net_costs / unmodified_dataset[program].program_cost;
        }

        if (disabled_effects[program]["willingness_to_pay"].length > 0) {
            for (disabled_effect of disabled_effects[program]["willingness_to_pay"]) {
                willingness_to_pay -= unmodified_dataset[program][disabled_effect];
                effect_disabled = true;
            }
            willingness_to_pay_per_program_cost = willingness_to_pay / unmodified_dataset[program].program_cost;
        }

        if (effect_disabled) {
            // Update mvpf if necessary!
            if (government_net_costs < 0 && willingness_to_pay > 0) {
                mvpf = "Inf";
            }
            else {
                mvpf = willingness_to_pay / government_net_costs;
            }
        }

        return({
            mvpf: mvpf,
            government_net_costs: government_net_costs,
            willingness_to_pay: willingness_to_pay,
            government_net_costs_per_program_cost: government_net_costs_per_program_cost,
            willingness_to_pay_per_program_cost: willingness_to_pay_per_program_cost,
            effect_disabled: effect_disabled
        })
}

function censorValues(datasets) {
    function censorMVPF(mvpf) {
        if (mvpf > infinity_cutoff) {
            return infinity_cutoff;
        }
        else if (mvpf < lower_cutoff) {
            return lower_cutoff;
        }
        else if (mvpf == "Inf") {
            return (infinity_cutoff + 1);
        }
        else {
            return mvpf;
        }
    }

    function censorCost(cost) {
        if (cost > cost_upper_cutoff ) {
            return cost_upper_cutoff;
        }
        else if (cost < cost_lower_cutoff ) {
            return cost_lower_cutoff;
        }
        else {
            return cost;
        }
    }

    function censorWTP(wtp) {
        if (wtp > wtp_upper_cutoff ) {
           return wtp_upper_cutoff;
        }
        else if (wtp < wtp_lower_cutoff ) {
            return wtp_lower_cutoff;
        }
        else {
            return wtp;
        }
    }

    for (let dataset of datasets) {
        for (let data of dataset.data) {
            data["mvpf"] = censorMVPF(data["mvpf"]);
            data["government_net_costs_per_program_cost"] = censorCost(data["government_net_costs_per_program_cost"]);
            data["willingness_to_pay_per_program_cost"] = censorWTP(data["willingness_to_pay_per_program_cost"]);
            if (data["mvpf_ci"]) {
                data["mvpf_ci"][0] = censorMVPF(data["mvpf_ci"][0]);
                data["mvpf_ci"][1] = censorMVPF(data["mvpf_ci"][1]);
            }
        }
    }
}

function generateEmptyDatasetScatter(dataset_label, dataset_number) {
    return ({
        label: dataset_label,
        data: [],
        backgroundColor: selectColor(dataset_number, true),
        borderColor: selectColor(dataset_number),
        pointHoverRadius: 7,
        pointRadius: 5
    });
}

function generateEmptyDatasetListCIPE(dataset_label, dataset_number) {
    return ({
        label: dataset_label,
        showLine: false,
        data: [],
        backgroundColor: selectColor(dataset_number),
        borderColor: selectColor(dataset_number),
        parsing: {
            xAxisKey: "mvpf",
            yAxisKey: "program_name"
        }
    });
}

function generateEmptyDatasetListCICI(dataset_label, dataset_number) {
    return ({
        label: dataset_label,
        data: [],
        backgroundColor: selectColor(dataset_number, true, 0.5),
        borderColor: selectColor(dataset_number, true, 0.5),
        parsing: {
            xAxisKey: "mvpf_ci",
            yAxisKey: "program_name"
        },
        type: "bar",
        barThickness: 10,
        borderRadius: Number.MAX_VALUE,
        borderSkipped: true,     
    });
}

function selectColor(number, background = false, overwrite_alpha_channel) {
    // returns a string in the format rgba(123,123,123,0.6)
    let background_opa = 0.7
    let foreground_opa = 0.9
    if (number > colors.length) {
        // console.log("Color not in range of supplied colors in colors.json");
        // Reuse last color in this case
        number = colors.length;
    }
    color_triple = colors[number - 1];
    if (!overwrite_alpha_channel) {
        return `rgba(${color_triple[0]},${color_triple[1]},${color_triple[2]},${background ? background_opa : foreground_opa})`;
    }
    return `rgba(${color_triple[0]},${color_triple[1]},${color_triple[2]},${overwrite_alpha_channel})`;
}

function getUnmodifiedProgram(program) {
    return(unmodified_dataset[program]);
}

function getVariableMapping(program) {
    return variable_mapping.find((current_program) => {
        return current_program.program === program;
    });
}

function changePriceLevel(from, to) {
    if (to > 2019) to = 2019;
    if (to < 1962) to = 1962;
    if (from > 2019) from = 2019;
    if (to < 1962) to = 1962;
    let multiplier = cpi[to] / cpi[from];
    return multiplier;
}

function generateBarChartProgramData(variable_to_plot, program, colorholder, adjust_prices_to_reform_year = false) {
    let datasets = [];
    let barComponents;
    let relevant_datapoint = getUnmodifiedProgram(program);

    let multiplier = adjust_prices_to_reform_year ? changePriceLevel(from = 2010, to = relevant_datapoint["prices_year"]) : 1

    if (variable_to_plot === "mvpf") {
        datasets.push({
            label: "Government Net Cost",
            data: [parseFloat(relevant_datapoint["government_net_costs"]) * multiplier],
            backgroundColor: selectColor(colorholder.color, true),
            borderColor: selectColor(colorholder.color),
        });
        datasets.push({
            label: "Willingness to Pay",
            data: [relevant_datapoint["willingness_to_pay"] * multiplier],
            backgroundColor: selectColor(colorholder.color + 1, true),
            borderColor: selectColor(colorholder.color + 1),
        });
        colorholder.color += 2;
        return (datasets);
    }
    barComponents = getVariableMapping(program)[variable_to_plot];

    for (let component in barComponents) {
        let ci_upper = relevant_datapoint[component + "_95ci_upper"] ? relevant_datapoint[component + "_95ci_upper"] * multiplier : "";
        let ci_lower = relevant_datapoint[component + "_95ci_lower"] ? relevant_datapoint[component + "_95ci_lower"] * multiplier : "";
        datasets.push({
            label: barComponents[component],
            effect_key: component,
            ci_upper: ci_upper,
            ci_lower: ci_lower,
            data: [relevant_datapoint[component] * multiplier],
            hidden: false,
            backgroundColor: selectColor(colorholder.color, true),
            borderColor: selectColor(colorholder.color),
            borderRadius: 4
        });
        colorholder.color++;
    }
    return (datasets);
}

function mvpfToString(mvpf) {
    if (mvpf == "Inf") {
        return "∞";
    }
    else if (mvpf == "-Inf") {
        return "-∞";
    }
    else {
        return parseFloat(mvpf).toFixed(2);
    }
}

// Trigger load of the default dataset:
async function loadData() {
    await Promise.all([
        loadJSON(document_root + "data/programs.json"),
        loadJSON(document_root + "data/categories.json")
    ]).then(return_values => {
        additional_program_info = return_values[0];
        categories = return_values[1];
        return Promise.all([
            readjson("default"),
            loadJSON(document_root + "data/variable_mapping.json"),
            loadJSON(document_root + "data/colors.json"),
            loadJSON(document_root + "data/literature.json")
        ]);
    }).then((return_values) => {
        let json = return_values[0];
        variable_mapping = return_values[1];
        colors = return_values[2];
        literature = return_values[3];
        json_as_array_copy = JSON.parse(JSON.stringify(json));
        initDisabledEffects(json);
    });
}