This section shows how to begin with **0. Data input** and proceed with the assumption checks (Shapiro–Wilk test or Levene’s test).
Please run all functions up to: 

```r
data <- ImportData()
```

Then from this point onward, all tests (1–8) will use the data object.
Below is an illustration of how to:
1. Import the dataset
2. Perform the normality test with perform_normality_tests(data)
3. Perform the variance test with perform_levene_test(data)

**1.1 Run *data <- ImportData()* and select the input file**

<img width="498" height="222" alt="image" src="https://github.com/user-attachments/assets/c21aa442-f12f-44be-9733-3d5aeeca6243" />
<br><br>

**1.2 If your data file contains multiple sheets, enter 2 and choose the sheet you want**

<img width="755" height="130" alt="image" src="https://github.com/user-attachments/assets/92728110-91a3-4e30-8792-5d3e6dc0e430" />
<br><br>

**2.1 Run *perform_normality_tests(data)*, every column will be assigned to one number**

<img width="515" height="533" alt="image" src="https://github.com/user-attachments/assets/d991d496-be7e-475f-a36c-2f689c84e494" />
<br><br>

**2.2 For example, we would like to test the normality of Tumor_Size_mm, Gender, and Incidence_Rate_per_100K based on Economic_Classification**

<img width="554" height="457" alt="image" src="https://github.com/user-attachments/assets/00fc2eb1-d28f-45ed-a176-194435079aa3" />
<br><br>

**2.3 Type *y* and tap Enter to see available plots**

<p float="left">
<img src="https://github.com/user-attachments/assets/b40ae5ba-b549-4d8f-86c5-1676a47ca8b2" width="400"/>
<img src="https://github.com/user-attachments/assets/6456bc4c-6800-4637-b5db-7e12e911f51f" width="400"/>
</p>

