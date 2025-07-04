# Interactive-Data-Storytelling-with-R-Shiny

## Mahesh Divakaran


**Pre-Workshop Prerequisites Document**

**Training Program:** Interactive Data Storytelling with R Shiny\
**Dates:** July 4 & 5, 2025\
**Time:** 10:00 AM\
**Venue:** Library Hall, St. Thomas College Palai

---

### 🛠 Software Installation

Please ensure the following software is installed on your laptop prior to the workshop:

#### 1. R (Latest Version)

Download and install from: [https://cran.r-project.org/](https://cran.r-project.org/)

#### 2. RStudio (Latest Version)

Download and install from: [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

---

### 📦 Required R Packages

Please install the following R packages before the workshop. You can run this command in the R console:

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "shinythemes",
  "DT",
  "plotly",
  "shinydashboard",
  "shinyWidgets",
  "readr",
  "ggplot2"
))
```

These packages are essential for creating interactive dashboards, custom UI components, dynamic tables, and plots.


# Github Config

- Create an account or login to github.com
- Clone the repo "https://tinyurl.com/stpala"

#### This script sets up a new R package project with Git and GitHub integration.
usethis::use_git_config(user.name="xxxxxxxx", user.email="xxxxxxx@example.org")

#### Create a personal access token for GitHub
usethis::create_github_token()

Now you are set with Git

---

### 🗓 Workshop Outline

#### Day 1: Shiny Basics & User Interface

- Introduction to R Shiny
- Designing App Layouts
- Creating Interactive Inputs (sliders, dropdowns, radio buttons)
- Displaying Dynamic Outputs (tables, plots, text)

#### Day 2: App Logic & Deployment

- Understanding Reactivity and Reactive Expressions
- Integrating Data in Apps
- Advanced UI Features (tabs, themes, widgets)
- Deploying Shiny Applications using shinyapps.io or other platforms

---

