module scenes
  use raytracer, only:scene_new, scene
  use primatives, only:plane_new, sphere_new
  use shaders

  implicit none(type, external)
  public
  contains
  function rgb_test() result(ret)
    use, intrinsic::iso_fortran_env, only:uint8
    type(scene) :: ret
      ret = scene_new(&
        [0.0,0.0,0.0], [1.0,1.0,1.0],&
        uint([229, 221, 107, 255], kind=uint8),&
        [&!planes
          plane_new(&
            [-627.0,87.0,23.0],& !point
            [0.2,0.2,1.0],& !normal

            uint([0, 40, 158, 255], kind=uint8),& !color
            burningship&
          ),&

           plane_new(&
             [0.0, 200.0,-20.0],& !point
             [-0.2,-0.2,1.0],& !normal

             uint([101, 219, 236, 255],kind=uint8), & !color
              checkerboard&
           ),&

           plane_new(&
             [-279.0,-254.0,319.0],& !point
             [0.5, -0.1,0.1],& !normalt

             uint([79, 103, 0, 255],kind=uint8),& !color
              mandlebrot&
           ),&

           plane_new(&
             [-772.0, 338.0,955.0],& !point
             [-0.5,0.4,-1.0],& !normal

             uint([255, 127, 20, 255],kind=uint8),& !color
              powertower&
           )&
        ],&

        [& ! spheres
          sphere_new(&
            60.0,& ! radius
            [157.0, -103.0, 137.0],&

            uint([6,6, 120, 255],kind=uint8),& !color
            checkerboard&
          ),&

           sphere_new(&
             100.0,& ! radius
             [0.0, 200.0, 300.0],&

             uint([122,122, 255, 255],kind=uint8),& !color
              mandlebrot&
           ),&

           sphere_new(&
             150.0,& ! radius
             [317.0, 574.0, 173.0],&

             uint([122,122, 255, 255],kind=uint8),& !color
              burningship&
            )&
        ]&
      )
  end function rgb_test

  !vibecoded scenes my friend made, only here for testing, for which they are pretty good
  !i do not vibe code tho i must admit these are more coherent scnenes than rgb test.
  !anywho i dont vibecode. this wasnt mine. i, just shoving it in the repo to have more variety.
  !these names are super tryhard too. moire garden? fractal_planetarium? they are just some multi
  !colored planes and spheres with fractals drawn on them its not that deep bro
  function moire_garden() result(ret)
    use, intrinsic :: iso_fortran_env, only:uint8
    type(scene) :: ret
    unsigned(uint8), parameter :: none(4) = [0u, 0u, 0u, 255u] ! placeholder for rainbow
      ret = scene_new(&
        [0.0, 0.0, 0.0],&                        ! camera position
        [0.68, 0.68, 0.27],&                     ! camera direction (unit length)
        uint([40, 10, 25, 255], kind=uint8),&    ! sky: deep violet-black [B,G,R,A]
        [&! planes
          plane_new(&
            [1200.0, 1200.0, -150.0],&           ! floor; rings are centred on this point
            [0.001, 0.0, 1.0],&                  ! tiny tilt avoids NaN texture axes
            none,&
            rainbow&
          )&
        ],&
        [&! spheres: totem at the ring centre, each resting on the one below
          sphere_new(135.0, [1200.0, 1200.0, -15.0], none, rainbow),& ! magenta
          sphere_new(115.0, [1200.0, 1200.0, 205.0], none, rainbow),& ! red
          sphere_new(100.0, [1200.0, 1200.0, 395.0], none, rainbow),& ! cyan
          sphere_new( 90.0, [1200.0, 1200.0, 565.0], none, rainbow),& ! yellow
          sphere_new( 70.0, [1200.0, 1200.0, 710.0], none, rainbow),& ! blue
          ! floating bubbles circling the totem
          sphere_new( 40.0, [1800.0, 1200.0, 100.0], none, rainbow),& ! yellow
          sphere_new( 60.0, [1500.0, 1720.0, 250.0], none, rainbow),& ! cyan
          sphere_new( 95.0, [ 900.0, 1720.0,  50.0], none, rainbow),& ! green
          sphere_new(105.0, [ 600.0, 1200.0, 300.0], none, rainbow),& ! blue
          sphere_new( 80.0, [ 900.0,  680.0, 150.0], none, rainbow),& ! red
          sphere_new( 45.0, [1500.0,  680.0, 400.0], none, rainbow),& ! yellow
          ! the sun: a Mandelbrot glowing sunset orange
          sphere_new(400.0, [3500.0, 2000.0, 1500.0], uint([30, 130, 255, 255], kind=uint8), mandlebrot),&
          ! the moon: powertower in electric violet
          sphere_new(350.0, [1500.0, 3800.0, 1300.0], uint([255, 60, 180, 255], kind=uint8), powertower)&
        ]&
      )
  end function moire_garden

  function fractal_planetarium() result(ret)
    use, intrinsic :: iso_fortran_env, only:uint8
    type(scene) :: ret
      ret = scene_new(&
        [0.0, 0.0, 0.0],&                        ! camera position
        [0.68, 0.68, 0.27],&                     ! camera direction (unit length)
        uint([90, 40, 20, 255], kind=uint8),&    ! sky: deep twilight navy
        [&! planes
          plane_new(&
            [0.0, 0.0, -200.0],&                 ! floor
            [0.001, 0.0, 1.0],&                  ! tiny tilt avoids NaN texture axes
            uint([200, 225, 245, 255], kind=uint8),& ! warm cream
            checkerboard&
          )&
        ],&
        [&! spheres
          sphere_new(&                           ! the planet: molten gold
            400.0,&
            [1800.0, 1800.0, 500.0],&
            uint([40, 200, 255, 255], kind=uint8),&
            mandlebrot&
          ),&
          sphere_new(&                           ! moon 1: icy blue
            220.0,&
            [1100.0, 2300.0, 900.0],&
            uint([255, 220, 120, 255], kind=uint8),&
            burningship&
          ),&
          sphere_new(&                           ! moon 2: hot pink
            250.0,&
            [2600.0, 1000.0, 800.0],&
            uint([220, 90, 255, 255], kind=uint8),&
            powertower&
          ),&
          sphere_new(60.0, [450.0,  250.0, -140.0], uint([ 60,  40, 220, 255], kind=uint8), checkerboard),& ! ruby
          sphere_new(60.0, [600.0,  450.0, -140.0], uint([ 90, 200,  60, 255], kind=uint8), checkerboard),& ! emerald
          sphere_new(60.0, [700.0,  700.0, -140.0], uint([220, 110,  30, 255], kind=uint8), checkerboard),& ! sapphire
          sphere_new(60.0, [650.0,  950.0, -140.0], uint([ 40, 190, 245, 255], kind=uint8), checkerboard),& ! gold
          sphere_new(60.0, [500.0, 1150.0, -140.0], uint([200,  70, 150, 255], kind=uint8), checkerboard)&  ! amethyst
        ]&
      )
  end function fractal_planetarium
end module scenes
