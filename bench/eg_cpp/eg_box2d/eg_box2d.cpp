#include<assert.h>

class BasicData {
protected:
    int m_datatype;
    int Datatype_Point2D = 1;
};

class Point2D : public BasicData
{
public:
	/// The type of the stored x and y coordinates.
	typedef double value_type;

	/// The type of the stored x and y coordinates. An alias for value_type.
	typedef value_type floatingpoint_type;

private:
	value_type m_x;
	value_type m_y;
public:
	/** Constructs a null point, i.e. with coordinates (0.0, 0.0) */
	Point2D()
		: m_x(0)
		, m_y(0)
	{ m_datatype = Datatype_Point2D; }

	/** Constructs a point with the given coordinates (x, y). */
	Point2D(value_type x, value_type y)
		: m_x(x)
		, m_y(y)
	{ m_datatype = Datatype_Point2D; }

	/** Constructs a point from the given ScanPoint */
//	Point2D(const ScanPoint& p);

	/** Constructs a point from the given Point3D. Its z-coordinate will be ignored. */
//	explicit Point2D(const Point3D& p);

	// Estimate the memory usage of this object
	inline virtual const unsigned int getUsedMemory() const {return sizeof(*this);};

	/** \name Accessor methods for information about this point */
	//\{

	/** Returns true if this point is zero in terms of the machine
	 * precision, that is, its value is exactly zero or "almost
	 * zero". */
	bool isZero() const;

	/** Returns the x-coordinate of this point. */
	value_type getX() const { return m_x; }

	/** Returns the y-coordinate of this point. */
	value_type getY() const { return m_y; }

	/** Returns the Euclidian distance to the origin,
	 * sqrt(x^2+y^2). An alias for getDist(). */
	value_type dist() const;

	/** Returns the Euclidian distance to the origin,
	 * sqrt(x^2+y^2). */
	value_type getDist() const { return dist(); }

	/** Same as dist() */
	value_type length() const { return dist(); }

	/** Returns the polar angle of this point, which is the angle from
	 * the x-axis to this point. An alias for getAngle(). */
	value_type angle() const;

	/** Returns the polar angle of this point, which is the angle from
	 * the x-axis to this point. */
	value_type getAngle() const { return angle(); }

	/** Calculates the polar coordinates of this point and writes them
	 * to the given arguments. dist will contain the distance of this
	 * point to the origin, and angle will contain the polar angle
	 * from the x-axis to this point. */
	void toPolar(value_type& dist, value_type& angle) const;

	/** Returns the polar coordinates of this point as a pair of
	 * distance and angle. The returned value's component "first" is
	 * the distance of this point to the origin, and the returned
	 * value's component "second" is the polar angle from the x-axis
	 * to this point. You can use this as follows:
	 *
	 * std::pair<value_type,value_type> x = p.toPolar();
	 * cout << "Distance=" << x.first << "; Angle=" << x.second;
	 *
	 */
	//std::pair<value_type, value_type> toPolar() const;

	/**
	 * Calculates the normalized form of this vector (point is
	 * considered as a vector here) and returns it.  If the vector has
	 * zero length (isZero() returns true), it will be left unchanged.
	 */
	Point2D normalized() const;

	/** Rotate the given point by the given angle around (0,0) and returns the
	 * rotated point.
	 *
	 * This method implements an "active rotation matrix".
	 *
	 * In other words: A positive rotation of the point (1,0) by the
	 * angle +pi/2 results in the point (0,1). Watch out: If the
	 * rotation is defined differently, another point (0,-1) might
	 * have been expected, but this function is implemented to return
	 * (0,1) here. */
	Point2D rotated(value_type angle_rad) const;

	/// Returns the x/y components of this class, converted into a Point3D object with zero z-component.
//	Point3D toPoint3D() const;

	//\}

	/** \name Setter methods for changing this point */
	//\{

	/** Sets the x-coordinate of this point to the given x coordinate. */
	void setX(value_type x) { m_x = x; }

	/** Sets the y-coordinate of this point to the given y coordinate. */
	void setY(value_type y) { m_y = y; }

	/** Sets the x- and y-coordinates of this point to the given
	 * coordinates. */
	void setXY(value_type x, value_type y) { m_x = x; m_y = y; }

	/**
	 * Normalizes this vector (point is treated as a vector here) to length 1.0.
	 * If the vector has zero length (isZero() returns true), it will be left unchanged.
	 */
	void normalize();

	/**
	 * Rotates this point around the orign (0,0)
	 * Same as rotated, but modifies the content.
	 */
	void rotate(value_type angle);

	/** Sets the coordinates of this point from the given polar
	 * coordinates. */
	void setPolar(value_type dist, value_type angle);

	/** Multiplies this point's coordinates by the given factor, and
	 * returns a reference to this point. */
	Point2D & operator*= ( value_type factor );

	/** Adds the given point to this point and returns a reference to
	 * this point. */
	Point2D & operator+= ( const Point2D & point );

	/** Subtracts the given point from this point and returns a
	 * reference to this point. */
	Point2D & operator-= ( const Point2D & point );

	/** Divides both x and y by the given divisor, and returns a
	 * reference to this point. */
	Point2D & operator/= ( value_type divisor );

	//\}


	/** \name Geometrical relations to other objects */
	//\{

	/** Returns the Euclidian distance to the given point,
	 * sqrt( (x_1-x_2)^2 + (y_1-y_2)^2 ). */
	value_type dist( const Point2D & point ) const;

	/** Returns the Euclidian distance to the given point,
	 * sqrt( (x_1-x_2)^2 + (y_1-y_2)^2 ). */
//	value_type dist( const ScanPoint & point ) const;

	/** Returns the square of the Euclidian distance to the given Point,
	 * (x_1-x_2)^2 + (y_1-y_2)^2.
	 */
	value_type distSquare ( const Point2D & point ) const;

	/** Returns the angle between this and another vector */
	value_type angle(const Point2D& point) const;

	//\}

	/** \name Serialization */
	//\{

	/// Reads a Point2D from an input stream
	/**
	 * \param is The input stream
	 * \param version 1 == compressed meter values, 4 bytes total; 2 == float values, 8 bytes total
	 */
	//std::istream& read (std::istream& is, unsigned int version);

	/// Reads a Point2D from a memory buffer and increments the buffer pointer
	/**
	 * \param buf The memory buffer
	 * \param version 1 == compressed meter values, 4 bytes total; 2 == float values, 8 bytes total
	 */
	void read (const char*& buf, unsigned int version);

	/// Writes a Point2D to an output stream
	/**
	 * \param os The output stream
	 * \param version 1 == compressed meter values, 4 bytes total; 2 == float values, 8 bytes total
	 */
	//std::ostream& write (std::ostream& os, unsigned int version) const;

	/// Writes a Point2D to a memory buffer and increments the buffer pointer
	/**
	 * \param buf The memory buffer
	 * \param version 1 == compressed meter values, 4 bytes total; 2 == float values, 8 bytes total
	 */
	void write (char*& buf, unsigned int version) const;

	/// Text output for debugging
	//std::string toString(UINT16 digits = 2) const;

	//\}

	/** Returns a newly constructed Point2D that is a cartesian
	 * representation of the given polar coordinates. */
	static Point2D fromPolar(value_type dist, value_type angle);

	friend inline bool operator==(const Point2D &, const Point2D &);
	friend inline bool operator!=(const Point2D &, const Point2D &);
	friend inline const Point2D operator+(const Point2D &, const Point2D &);
	friend inline const Point2D operator-(const Point2D &, const Point2D &);
	friend inline const Point2D operator*(value_type, const Point2D &);
	friend inline const Point2D operator*(const Point2D &, value_type);
	friend inline Point2D::value_type operator*(const Point2D &p1, const Point2D &p2);
	friend inline const Point2D operator-(const Point2D &);
	friend inline const Point2D operator/(const Point2D &, value_type);
};

class Box2D : public BasicData
{
public:
	/// The type of the stored x, y coordinates, and the rotation.
	typedef Point2D::value_type value_type;
private:
	Point2D m_center;
	Point2D m_size;
	value_type m_rotation;
public:
	/// Constructor for an all-zero Box2D.
	Box2D();

	/// Constructor with specified center point, size, and rotation.
	/**
	 * \note The size components must be non-negative, otherwise an assertion will fail.
	 *
	 * \param center The center point
	 * \param size The size of the box in the box' coordinate system. Must be non-negative.
	 *
	 * \param rotation The rotation of the box' coordinate system
	 * around its center point in [radians]. Must be in the interval
	 * [-pi,pi], which can be obtained by using
	 * ::normalizeRadians().
	 */
	Box2D(const Point2D& center, const Point2D& size, value_type rotation = 0.0);

	/// Constructor with all values given.
	/**
	 * Constructor with all values given: x/y of center point, x/y of
	 * size (where the x coordinate of the size is in the same
	 * direction as the x axis of the coordinate system, rotated by
	 * the rotation argument; the y coordinate accordingly), and
	 * rotation.
	 *
	 * \note The x_size and y_size must be non-negative.
	 *
	 * \param x_center X-coordinate of center point
	 * \param y_center Y-coordinate of center point
	 *
	 * \param x_size The size of the box in X-direction in the box' coordinate system. Must be non-negative.
	 * \param y_size The size of the box in Y-direction in the box' coordinate system. Must be non-negative.
	 *
	 * \param rotation The rotation of the box' coordinate system
	 * around its center point in [radians]. Must be in the interval
	 * [-pi,pi], which can be obtained by using
	 * ::normalizeRadians().
	 */
	Box2D(value_type x_center, value_type y_center, value_type x_size, value_type y_size, value_type rotation = 0.0);

	// Estimate the memory usage of this object
	virtual const unsigned int getUsedMemory() const { return sizeof(*this); };


	/** \name Accessor methods for information about this box */
	//\{

	/// Returns the center point of this Box
	const Point2D& getCenter() const { return m_center; }

	/// Returns the size of this Box
	/**
	 * The returned size denotes the size of the box in x-direction
	 * Point2D::getX() and y-direction Point2D::getY(), where the
	 * x-direction is rotated against the original x-axis by
	 * getRotation().
	 */
	const Point2D& getSize() const { return m_size; }

	/** Returns the rotation angle of this Box2D in [radians], counter clock wise.
	 *
	 * The rotation of the box' coordinate system around its center
	 * point. Must be in the interval [-pi,pi]
	 */
	value_type getRotation() const { return m_rotation; }

	/// Converts this Box2D to a closed polygon
	/**
	 * Converts this Box2D to a closed polygon with 5 points, where
	 * the last point is identical to the first one to make it
	 * closed.  */
	//Polygon2D toPolygon() const;

	/// Returns a Box in parallel to the coordinate system that bounds this box
	/**
	 * This function calculates a bounding box to the given box,
	 * because the given one might be rotated into some other
	 * direction. In contrast to this, the returned box will have zero
	 * rotation and will be in parallel to the coordinate system.
	 */
	Box2D toBoundingBox() const;

	/** \brief Returns boundary angles for this box.
	 *
	 * This function calculates a low and a high boundary angle for
	 * all edges of the given (rotated) Box2D. The returned FloatPair
	 * has the component "first" for the lower bounding angle, and
	 * "second" for the upper bounding angle.
	 *
	 * (Note: This ordering is swapped compared to the scan point
	 * ordering!)
	 */
	//std::pair<value_type, value_type> getBoundingAngles() const;

	/** \brief Returns a Box that is copied from this one but with its center point moved
	 */
	Box2D movedBy(const Point2D& centerMovement) const;
	//\}


	/** \name Setter methods for changing this box */
	//\{

	/// Sets the center point of this Box2D
	void setCenter(const Point2D& p) { m_center = p; }

	/// Sets the center point of this Box2D
	void setCenter(value_type x, value_type y) { m_center.setXY(x, y); }

	/// Sets the size of this Box. Must be non-negative.
	void setSize(const Point2D& p);

	/// Sets the size of this Box2D. Must be non-negative.
	void setSize(value_type x_length, value_type y_width);

	/** \brief Sets the rotation angle of this Box in [radians], counter
	 * clock wise.
	 *
	 * The rotation of the box' coordinate system around its center
	 * point. Must be in the interval [-pi,pi], which can be obtained
	 * by using ::normalizeRadians().
	 */
	void setRotation(value_type r);

	/// Move the center point of this box by the given point values.
	void moveBy(const Point2D& centerMovement);
	//\}

	/** \name Geometrical relations to other objects */
	//\{

	/// Returns true if the given Point2D is inside this box or on its outline.
	/**
	 * (Note: This function is relatively cheap - it needs two sinus
	 * operations, four multiplications and a bunch of
	 * comparisons.)
	 */
	bool containsPoint(const Point2D& point) const;

	/// Returns the distance from the outline of this Box2D to the given point.
	/**
	 * This function calculates the minimum distance over the
	 * distances from the given point to each of the four outside
	 * lines of this Box2D.
	 *
	 * Internally, this might be implemented using
	 * Polygon2D::distanceToPoint and
	 * Line2D::distanceFromLineSegment(), but maybe the implementation
	 * will be optimized to work in some other way.
	 */
	Point2D::value_type distanceFromOutline(const Point2D& point) const;

	/// Returns the mean distance from the outline of this Box2D to all points of the point vector.
	/**
	 * For each given point, this function calculates the minimum
	 * distance over the distances from the given point to each of the
	 * four outside lines of this Box2D and returns that minimum
	 * distance. The returned value is the mean value of all
	 * distances.
	 *
	 * This is an overloaded version of distanceFromOutline(const
	 * Point2D&) for your convenience.
	 */
	//Point2D::value_type distanceFromOutline(const std::vector<Point2D>& points) const;

	/// Returns the mean distance from the outline of this Box2D to all points of the given point iterator range.
	/**
	 * For each given point, this function calculates the minimum
	 * distance over the distances from the given point to each of the
	 * four outside lines of this Box2D and returns that minimum
	 * distance. The returned value is the mean value of all
	 * distances.
	 *
	 * This is an overloaded version of distanceFromOutline(const
	 * Point2D&) for your convenience.
	 */
	//Point2D::value_type distanceFromOutline(const std::vector<Point2D>::const_iterator& begin,
	//										const std::vector<Point2D>::const_iterator& end) const;


	/// Returns an orientated bounding box for the given list of points.
	/**
	 * Given a list of points and a fixed orientation, this function
	 * will calculate a bounding box for the points that has the given
	 * orientation.
	 */
	//static Box2D orientatedBox(value_type orientation_rad, const Polygon2D& poly);

	/// Returns an orientated bounding box for the given list of points.
	/**
	 * Given a list of points and a fixed orientation, this function
	 * will calculate a bounding box for the points that has the given
	 * orientation.
	 *
	 * This is an overloaded version of orientatedBox(const
	 * Polygon2D&) for your convenience.
	 */
	//static Box2D orientatedBox(value_type orientation_rad, const std::vector<Point2D>& points);

	/// Returns an orientated bounding box for the given list of points.
	/**
	 * Given a list of points and a fixed orientation, this function
	 * will calculate a bounding box for the points that has the given
	 * orientation.
	 *
	 * This is an overloaded version of orientatedBox(const
	 * Polygon2D&) for your convenience.
	 */
	//static Box2D orientatedBox(value_type orientation_rad,
	//						   const std::vector<Point2D>::const_iterator& begin,
	//						   const std::vector<Point2D>::const_iterator& end);


	//std::string toString() const;		// Konvertierung in String


	friend inline bool operator==(const Box2D &, const Box2D &);
	friend inline bool operator!=(const Box2D &, const Box2D &);

private:
	void verifyNumericRanges();

};

int abs(int x){
    if (x<0)
        return -x;
    else
        return x;
}
bool isNaN(int x){
    return false;
}
bool fuzzyCompare(int x, int y){
    return true;
}
int normalizeRadians(int x){
    return x;
}
void Box2D::verifyNumericRanges()
{

	bool correctRange = true;

	// Check our assumptions about the m_size
	if (m_size.getX() < 0 || m_size.getY() < 0)
	{
		//printWarning("Size of Box2D was given as negative value (" +
		//				 ::toString(m_size.getX(), 2) + "," + ::toString(m_size.getY(), 2) +
		//				 ") - silently using the absolute value instead.");
		m_size.setX(abs(m_size.getX()));
		m_size.setY(abs(m_size.getY()));
		correctRange = false;
		// This is probably better than throwing an exception.
		//throw std::out_of_range("Size of Box2D negative - this is an invalid Box2D.");
	}

	if (isNaN(m_size.getX()))
	{
		m_size.setX(0);
		//printWarning("Size.getX() of Box2D was given as NaN value - silently using Zero instead.");
		correctRange = false;
	}
	if (isNaN(m_size.getY()))
	{
		m_size.setY(0);
		//printWarning("Size.getY() of Box2D was given as NaN value - silently using Zero instead.");
		correctRange = false;
	}
	if (isNaN(m_rotation))
	{
		m_rotation = 0;
		//printWarning("Rotation of Box2D was given as NaN value - silently using Zero instead.");
		correctRange = false;
	}
	value_type normd_rot = normalizeRadians(m_rotation);
	if (!fuzzyCompare(normd_rot, m_rotation))
	{
		//printWarning("Rotation of Box2D (" + ::toString(m_rotation, 2) + ") was outside of its [-pi,pi] definition range - silently normalizing it back into that range (" +
		//					::toString(normd_rot, 2) + ").");
		m_rotation = normd_rot;
		correctRange = false;
	}

	assert(correctRange);

}

