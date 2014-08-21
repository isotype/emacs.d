/**
 * LightSensor
 *
 * Extracted from http://stackoverflow.com/questions/10061028/isight-ambient-sensor
 **/

#include <stdio.h>
#include <IOKit/IOKitLib.h>

#import <Foundation/Foundation.h>

int getLightLevel()
{

  uint32_t outputs = 2;
  uint64_t values[outputs];

  io_connect_t port = 0;

  // Get the light sensor service
  io_service_t lightSensor = IOServiceGetMatchingService(kIOMasterPortDefault,
                                                         IOServiceMatching("AppleLMUController"));

  // Connect the service at the given port
  IOServiceOpen(lightSensor, mach_task_self(), 0, &port);

  // Access it, assigning the levels the sensors measure to `values`
  IOConnectCallMethod(port, 0, nil, 0, nil, 0, values, &outputs, nil, 0);

  // Disconnects our service
  IOConnectRelease(lightSensor);

  // Return the first value (I believe this is the iSight camera)
  return (int)values[0];

}

int main (int argc, char *argv[])
{
  fprintf(stdout, "%i", getLightLevel() );
  return 0;
}
