Drone Simulation task: 

Steps to perform Simulation:
1.Load the beams(simulator.beam, get_soordinates.beam) into a directory.

2.Run the following command in an erlang shell to start the simulation: "simulator:simulate("6043.csv","5937.csv","tube.csv")."

3.After some time following statement will be seen:
   1> simulator:simulate("6043.csv","5937.csv","tube.csv").
   <0.43.0>
   simulation shutdown for drone_5937
   simulation shutdown for drone_6043
 which shows the simulation is ended.

4.To stop the simulation in between, run the following command:
   drone_1 ! shutdown.
   drone_2 ! shutdown.

5.After completion of simulation the reports and log files are:
  5.1 6043_report.txt  - contains the report as per the task
  5.2 6043_Communication.log - contains the communication between the dispatcher and drone1
  5.3 5937_report.txt  - contains the report as per the task
  5.4 5937_Communication.log - contains the communication between the dispatcher and drone2



