# COBOL implementation

Based on code from <https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeCOBOL/solution_1>

I took the above code and made some minor changes to properly compile on a real mainframe.

## Run instructions

Create a partition dataset (I called it &SYSUID.DGARAGE). Inside this dataset create two members: JCL and PRIMES.

Submit the JCL job and wait. After the job is completed, the member OUTPUT will appear in the above dataset.

## Output

Below is an example of the output from IBM Mainframe made avaliable for Master the Mainframe 2020 project.

```bash
Passes: 02355, Time: 50, Avg: 000212 (sec/pass), Limit: 1000000, Count: 0078498,
 Valid: True

fvbakel_Cobol;02355;50;1;algorithm=base,faithful=no,bits=8
```
