
class MrEdBoundaryClass
{
 public:
  void *my_address;
 
  MrEdBoundaryClass(); 
};

extern MrEdBoundaryClass _alpha_, _omega_;

extern int go_alpha_boundary();
extern int go_omega_boundary();
