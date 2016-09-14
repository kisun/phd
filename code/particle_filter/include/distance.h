#define R 6371000;

/**
 * Convert degrees to radians
 * @param  deg value in degrees
 * @return     double, value in radians
 */
double deg2rad(double deg) {
  return (deg * M_PI / 180);
}

/**
 * Convert radians to degrees
 * @param  deg value in radians
 * @return     double, value in degrees
 */
double rad2deg(double rad) {
  return (rad * 180 / M_PI);
}

/**
 * Compute the distance between two coordinates, in meters
 * @param  lat1 latitude value of the first point
 * @param  lon1 longitude value of the first point
 * @param  lat2 latitude value of the second point
 * @param  lon2 longitude value of the second point
 * @return      double, the distance, in meters, between the points
 */
double distance(double lat1, double lon1, double lat2, double lon2) {
  if (lat1 == lat2 && lon1 == lon2) {
    return 0;
  }
  
  double theta, dist;
  // double R = 6371000;

  theta = lon1 - lon2;
  dist = sin(deg2rad(lat1)) * sin(deg2rad(lat2)) +
         cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * cos(deg2rad(theta));
  dist = acos(dist);
  dist = dist * R;

  return dist;
}

/**
 * Compute the bearing from one coordinate to the next, in degrees
 * @param  lat1 latitude value of the first point
 * @param  lon1 longitude value of the first point
 * @param  lat2 latitude value of the second point
 * @param  lon2 longitude value of the second point
 * @return      double, the bearing, in degrees, between the points
 */
double bearing(double lat1, double lon1, double lat2, double lon2) {
  double delta, x, y, theta;

  delta = lon2 - lon1;
  y = sin(deg2rad(delta)) * cos(deg2rad(lat2));
  x = cos(deg2rad(lat1)) * sin(deg2rad(lat2)) -
      sin(deg2rad(lat1)) * cos(deg2rad(lat2)) * cos(deg2rad(delta));
  theta = rad2deg(atan2(y, x));

  theta = fmod(theta + 360, 360);

  return theta;
}
