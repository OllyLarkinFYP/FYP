# VIDE: A Verilog IDE for novices

VIDE is a VSCode extension and a backend application that serves it to allow the editing and simulation of Verilog projects. A user guide can be found in chapter 9 of the report.

## Project Structure

-   `back-end`: Contains the backend application code and tests that. The backend provides parsing, compiling, and simulation capabilities.
-   `front-end`: The front end contains the VSCode application that interfaces with the backend.

## Build Instructions

### Backend

To get API declaration:

```
cd back-end/CoreLogic
dotnet run -- -d
```

To process a job:

```
cs back-end/CoreLogic
dotnet run -- -j <job>
```

Specifics on the format of the job can be found in the report.

## Frontend

To run:

Open `front-end/vide` in VSCode and then run `npm run watch` in a terminal. Press `F5` in the VSCode window to open a test session of VSCode with the extension installed.
