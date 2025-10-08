# 🌱 **FLORA — See the Earth Breathe**

> *Explore, analyze, and predict vegetation health with NDVI intelligence.*

---

## 🛰️ **How FLORA Works**

FLORA is structured into four main components:

1. **Map and NDVI**
2. **NDVI Prediction**
3. **NDVI Visualization**
4. **Timeline**

---

## 🗺️ **1. Map and NDVI**

When the application starts, an on-screen tutorial guides the user through the available tools.

On the **left-hand side** of the map, users will find tools for:
- 🔍 **Zoom**
- 🟩 **Selection box**
- 🧹 **Clear all**

These tools make it easy to navigate and interact with the map.  
The **selection box** allows users to define an area of interest to calculate its **NDVI value**.

Once an area is selected, the **NDVI value** appears on the left panel, displayed through a color-coded scale:

- 🔴 **Red:** Low vegetation health  
- 🟡 **Yellow:** Moderate vegetation health  
- 🟢 **Green:** Healthy vegetation  

Results can be **exported in CSV format** for further analysis.

The interactive map offers three visualization modes:
- 🗺️ **Standard map**
- 🛰️ **Satellite view**
- 🏔️ **Topographic (relief) view**

---

## 📈 **2. NDVI Prediction**

To generate a prediction:
1. Select a **specific area**.
2. Define the **number of months** to forecast.

FLORA automatically produces **dynamic graphs** showing the expected NDVI evolution over time,  
accompanied by an **interpretive text** that explains the results in *simple and accessible language*.

---

## 🌾 **3. NDVI Visualization**

This graph displays how the **average NDVI** has evolved over time.

- **Y-axis:** Average NDVI value *(from 0 to 1)*  
- **X-axis:** Time  

### 📊 **Interpretation**
- 🔼 *If both trend lines (blue and red) increase → vegetation health is improving.*
- 🔽 *If both decrease → vegetation is deteriorating.*

This visualization helps users easily **observe vegetation changes** and **understand field conditions**.

---

### 🔍 **NDVI Series Decomposition**

To analyze the data in greater detail, FLORA breaks the time series into simpler components:

- 🟦 **Original Data:** Actual observed NDVI values (same as in the main graph).  
- ⚪ **Residuals:** Small random variations or “noise” from environmental changes.  
- 🔁 **Seasonality:** Annual patterns (e.g., seasonal vegetation changes).  
- 🔺 **Trend:** Shows whether vegetation is generally improving or declining.

**Interpretation:**  
This breakdown helps identify **seasonal effects** and **unusual events** — like *wildfires or droughts* — that disrupt normal vegetation behavior.

---

### 📉 **NDVI Prediction Chart**

This chart illustrates both **past vegetation changes** and **future projections**:

- ⚫ **Black line:** Actual observed NDVI values *(historical data)*  
- 🔵 **Blue line:** Predicted NDVI evolution *(forecast)*  
- ⚪ **Gray area:** Confidence interval *(uncertainty range)*

This predictive view helps **anticipate vegetation changes** and plan **preventive or improvement actions**.

---

## 🕒 **4. Timeline**

This section enables users to **analyze NDVI evolution over time** in selected areas.

### ⚙️ **Main functions**
- 🟩 **Select Area (rect_id):** Choose the specific zone to analyze.  
- 📅 **Date Range:** Filter data for a selected time interval.  
- 📈 **Line Graph:** Displays how NDVI changes over time.

### 🎨 **Color Indicators**
- 🟢 **Green:** High NDVI → Healthy vegetation  
- 🟠 **Orange:** Moderate NDVI → Average vegetation vigor  
- 🔴 **Red:** Low NDVI → Poor or scarce vegetation

### 💬 **Tooltip**
When hovering over each point, messages appear showing:
- The **NDVI value**
- Its **status** (healthy, moderate, or weak)

A **high NDVI (≈ 1)** → Dense and healthy vegetation  
A **low NDVI (≈ 0 or negative)** → Weak or damaged vegetation  

The combination of **color-coded points** and **interactive tooltips** helps users quickly detect vegetation changes and potential alerts over time.

---

🌎 *FLORA is more than a visualization tool — it’s a window into the living pulse of our planet.*
