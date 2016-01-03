SELECT pitcher, pitchType, home as stadium, 
	(case when battedBallType = "FB" then 1 else 0 end) as FB,
    (case when paResult = "HR" then 1 else 0 end) as HR,
    pitcherHand,batterHand,
	(case when pitcherHand = batterHand then 1 else 0 end) as sameHand,
    (case when manOnFirst = "TRUE" or manOnSecond = "TRUE" or
		manOnThird = "TRUE" then 1 else 0 end) as man_on_base,
        
	spinRate, SpinDir, releaseVelocity,
    (px-x0) as x_mov,
    (pz-z0) as z_mov,
    sqrt(power(px-x0,2) + power(pz-z0,2)) as tot_mov,
    (case when pz > (szt + szb)/2 then 1 else 0 end) as top_of_zone,
    (case when (px < 0 and batterHand = "R") or (px > 0 and batterHand = "L")
		then 1 else 0 end) as inside,
    vx0, vy0, vz0, 
    ax, ay, az, 
    px,pz,
    szt,szb,
    battedBallAngle, battedBallDistance
FROM trumedia_hackathon.pitches_2013 where pitchResult = "IP" /*this line changes to generate each year*/
;