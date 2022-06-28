#include <assert.h>

using namespace std;

class Box {
   public:
      double length;   // Length of a box
      double breadth;  // Breadth of a box
      double height;   // Height of a box
};

int main() {
   Box Box1;        // Declare Box1 of type Box
   Box Box2;        // Declare Box2 of type Box
   double volume = 0.0;     // Store the volume of a box here

   // box 1 specification
   Box1.height = 5.0;
   Box1.length = 6.0;
   Box1.breadth = 7.0;

   // box 2 specification
   Box2.height = 10.0;
   Box2.length = 12.0;
   Box2.breadth = 2.0;

   // volume of box 1
   volume = Box1.height * Box1.length * Box1.breadth;
   assert(volume == 210.0);

   // volume of box 2
   volume = Box2.height * Box2.length * Box2.breadth;
   assert(volume == 240.0);
   return (int)volume;
}

