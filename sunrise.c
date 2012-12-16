/* Copyright (GPL) 2004   Mike Chirico mchirico@comcast.net
   Updated: Sun Nov 28 15:15:05 EST 2004

   Program adapted by Mike Chirico mchirico@comcast.net

   Reference:
    http://prdownloads.sourceforge.net/souptonuts/working_with_time.tar.gz?download
    http://www.srrb.noaa.gov/highlights/sunrise/sunrise.html


  Compile:

     gcc -o sunrise -Wall -W -O2 -s -pipe -lm sunrise.c

  or for debug output

     gcc -o sunrise -DDEBUG=1 -Wall -W -O2 -s -pipe -lm sunrise.c




  This can also go in a batch job to calculate the next
  20 days as follows:

    #!/bin/bash
    lat=39.95
    long=75.15
    for (( i=0; i <= 20; i++))
    do
     ./sunrise    `date -d "+$i day" "+%Y %m %d"` $lat $long
    done


   
*/

/* gcc -DDEBUG=1 .. */
#ifndef DEBUG
#define DEBUG 0
#endif


#include <sys/time.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>






double calcSunEqOfCenter(double t);


/* Convert degree angle to radians */

double  degToRad(double angleDeg)
{
  return (M_PI * angleDeg / 180.0);
}

double radToDeg(double angleRad)
{
  return (180.0 * angleRad / M_PI);
}


double calcMeanObliquityOfEcliptic(double t)
{
  double seconds = 21.448 - t*(46.8150 + t*(0.00059 - t*(0.001813)));
  double e0 = 23.0 + (26.0 + (seconds/60.0))/60.0;

  return e0;              // in degrees
}


double calcGeomMeanLongSun(double t)
{


  double L = 280.46646 + t * (36000.76983 + 0.0003032 * t);
  while( (int) L >  360 )
    {
      L -= 360.0;

    }
  while(  L <  0)
    {
      L += 360.0;

    }


  return L;              // in degrees
}






double calcObliquityCorrection(double t)
{
  double e0 = calcMeanObliquityOfEcliptic(t);


  double omega = 125.04 - 1934.136 * t;
  double e = e0 + 0.00256 * cos(degToRad(omega));
  return e;               // in degrees
}

double calcEccentricityEarthOrbit(double t)
{
  double e = 0.016708634 - t * (0.000042037 + 0.0000001267 * t);
  return e;               // unitless
}

double calcGeomMeanAnomalySun(double t)
{
  double M = 357.52911 + t * (35999.05029 - 0.0001537 * t);
  return M;               // in degrees
}


double calcEquationOfTime(double t)
{


  double epsilon = calcObliquityCorrection(t);               
  double  l0 = calcGeomMeanLongSun(t);
  double e = calcEccentricityEarthOrbit(t);
  double m = calcGeomMeanAnomalySun(t);
  double y = tan(degToRad(epsilon)/2.0);
  y *= y;
  double sin2l0 = sin(2.0 * degToRad(l0));
  double sinm   = sin(degToRad(m));
  double cos2l0 = cos(2.0 * degToRad(l0));
  double sin4l0 = sin(4.0 * degToRad(l0));
  double sin2m  = sin(2.0 * degToRad(m));
  double Etime = y * sin2l0 - 2.0 * e * sinm + 4.0 * e * y * sinm * cos2l0
				- 0.5 * y * y * sin4l0 - 1.25 * e * e * sin2m;


  return radToDeg(Etime)*4.0;	// in minutes of time


	}


double calcTimeJulianCent(double jd)
{

  double T = ( jd - 2451545.0)/36525.0;
  return T;
}









double calcSunTrueLong(double t)
{
  double l0 = calcGeomMeanLongSun(t);
  double c = calcSunEqOfCenter(t);

  double O = l0 + c;
  return O;               // in degrees
}



double calcSunApparentLong(double t)
{
  double o = calcSunTrueLong(t);

  double  omega = 125.04 - 1934.136 * t;
  double  lambda = o - 0.00569 - 0.00478 * sin(degToRad(omega));
  return lambda;          // in degrees
}




double calcSunDeclination(double t)
{
  double e = calcObliquityCorrection(t);
  double lambda = calcSunApparentLong(t);

  double sint = sin(degToRad(e)) * sin(degToRad(lambda));
  double theta = radToDeg(asin(sint));
  return theta;           // in degrees
}


double calcHourAngleSunrise(double lat, double solarDec)
{
  double latRad = degToRad(lat);
  double sdRad  = degToRad(solarDec);



  double HA = (acos(cos(degToRad(90.833))/(cos(latRad)*cos(sdRad))-tan(latRad) * tan(sdRad)));

  return HA;              // in radians
}

double calcHourAngleSunset(double lat, double solarDec)
{
  double latRad = degToRad(lat);
  double sdRad  = degToRad(solarDec);


  double HA = (acos(cos(degToRad(90.833))/(cos(latRad)*cos(sdRad))-tan(latRad) * tan(sdRad)));

  return -HA;              // in radians
}


double calcJD(int year,int month,int day)
	{
		if (month <= 2) {
			year -= 1;
			month += 12;
		}
		int A = floor(year/100);
		int B = 2 - A + floor(A/4);

		double JD = floor(365.25*(year + 4716)) + floor(30.6001*(month+1)) + day + B - 1524.5;
		return JD;
	}




double calcJDFromJulianCent(double t)
{
  double JD = t * 36525.0 + 2451545.0;
  return JD;
}


double calcSunEqOfCenter(double t)
{
		double m = calcGeomMeanAnomalySun(t);

		double mrad = degToRad(m);
		double sinm = sin(mrad);
		double sin2m = sin(mrad+mrad);
		double sin3m = sin(mrad+mrad+mrad);

		double C = sinm * (1.914602 - t * (0.004817 + 0.000014 * t)) + sin2m * (0.019993 - 0.000101 * t) + sin3m * 0.000289;
		return C;		// in degrees
}






double calcSunriseUTC(double JD, double latitude, double longitude)
 {

	double t = calcTimeJulianCent(JD);

		// *** First pass to approximate sunrise


	double  eqTime = calcEquationOfTime(t);
	double  solarDec = calcSunDeclination(t);
	double  hourAngle = calcHourAngleSunrise(latitude, solarDec);
        double  delta = longitude - radToDeg(hourAngle);
	double  timeDiff = 4 * delta;	// in minutes of time	
	double  timeUTC = 720 + timeDiff - eqTime;	// in minutes	
        double  newt = calcTimeJulianCent(calcJDFromJulianCent(t) + timeUTC/1440.0); 


         eqTime = calcEquationOfTime(newt);
         solarDec = calcSunDeclination(newt);
		
		
		hourAngle = calcHourAngleSunrise(latitude, solarDec);
		delta = longitude - radToDeg(hourAngle);
		timeDiff = 4 * delta;
		timeUTC = 720 + timeDiff - eqTime; // in minutes



		return timeUTC;
	}

double calcSunsetUTC(double JD, double latitude, double longitude)
 {

	double t = calcTimeJulianCent(JD);

		// *** First pass to approximate sunset


	double  eqTime = calcEquationOfTime(t);
	double  solarDec = calcSunDeclination(t);
	double  hourAngle = calcHourAngleSunset(latitude, solarDec);
        double  delta = longitude - radToDeg(hourAngle);
	double  timeDiff = 4 * delta;	// in minutes of time	
	double  timeUTC = 720 + timeDiff - eqTime;	// in minutes	
        double  newt = calcTimeJulianCent(calcJDFromJulianCent(t) + timeUTC/1440.0); 


         eqTime = calcEquationOfTime(newt);
         solarDec = calcSunDeclination(newt);
		
		
		hourAngle = calcHourAngleSunset(latitude, solarDec);
		delta = longitude - radToDeg(hourAngle);
		timeDiff = 4 * delta;
		timeUTC = 720 + timeDiff - eqTime; // in minutes

		// printf("************ eqTime = %f  \nsolarDec = %f \ntimeUTC = %f\n\n",eqTime,solarDec,timeUTC);


		return timeUTC;
	}




/*
int main(int argc, char **argv)
{



  time_t seconds;
  time_t tseconds;
  struct tm  *ptm=NULL;
  struct tm  tm;


  int year=2004,month=8,day=21,dst=-1;
  char buffer[30];

  float JD=calcJD(year,month,day);
  double latitude = 39.95;  //convert to just degrees.  No min/sec
  double longitude = 75.15;


  if(argc == 6) {
    year=atoi(argv[1]);
    month=atoi(argv[2]);
    day=atoi(argv[3]);
    latitude = atof(argv[4]);
    longitude = atof(argv[5]);
    JD = calcJD(year,month,day);

  } else {
  printf("\nUsage: (note convert latitude and longitude to degrees) \n");
  printf("./sunrise year month date latitude longitude \n\n");
  printf(" Latitude and longitude must be in degrees and or fraction of degrees. Not min sec\n");
  printf("\n US Listings Of Latitude and Longitude\n");
  printf("    http://www.census.gov/geo/www/tiger/latlng.txt\n");
  printf("    But use positive values for longitude for above listing\n");

  printf("\n Example: (Just outside Philadelphia PA, USA)\n\n");
  printf("./sunrise 2004 8 21 39.95 75.15 \n");
  printf("./sunrise `date \"+%cY %cm %cd\"` 39.95 75.15\n\n",'%','%','%');
  }


 if(DEBUG)
  printf("Julian Date  %f \n",JD);
 if(DEBUG)
  printf("Sunrise timeUTC %lf \n", calcSunriseUTC( JD,  latitude,  longitude));
 if(DEBUG)
  printf("Sunset  timeUTC %lf \n", calcSunsetUTC( JD,  latitude,  longitude));



  tm.tm_year= year-1900;
  tm.tm_mon=month-1;  // Jan = 0, Feb = 1,.. Dec = 11 
  tm.tm_mday=day;
  tm.tm_hour=0;
  tm.tm_min=0;
  tm.tm_sec=0;
  tm.tm_isdst=-1;  

  seconds = mktime(&tm);
  if(DEBUG)
   printf("Number of seconds %ld\n",seconds);
  if(DEBUG)
   printf("Number of year %d\n",year);
  int delta;

  dst=tm.tm_isdst;


  ptm = gmtime ( &seconds );
  delta= ptm->tm_hour;

  if(DEBUG)
    printf("delta=%d dst=%d\n",delta,dst);
  
  tseconds= seconds;
  if(DEBUG)
   printf("Number of seconds %ld\n",seconds);
  seconds= seconds + calcSunriseUTC( JD,  latitude,  longitude)*60;
  seconds= seconds - delta*3600;




  strftime(buffer,30,"%m-%d-%Y  %T",localtime(&seconds));
  printf("Sunrise  %s   ",buffer);



  seconds=tseconds;
  seconds+=calcSunsetUTC( JD,  latitude,  longitude)*60;
  seconds= seconds - delta*3600;


  strftime(buffer,30,"%m-%d-%Y  %T",localtime(&seconds));
  printf("Sunset %s\n",buffer);




  return 0;

}
*/


