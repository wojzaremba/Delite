package ppl.apps.liszt.scalar_convection_rw

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object SCRRunner extends DeLisztApplicationRunner with SCR

trait SCR extends DeLisztApplication {
  var position : Rep[Field[Vertex,Vec[_3,Double]]] = null
  var interior_set : Rep[BoundarySet[Face]] = null
  var inlet_set : Rep[BoundarySet[Face]] = null
  var outlet_set : Rep[BoundarySet[Face]] = null
  var far_field_set : Rep[BoundarySet[Face]] = null
  
  var isInterior_face : Rep[Field[Face,Int]] = null
  var isOutlet_face : Rep[Field[Face,Int]] = null
  var isInlet_face : Rep[Field[Face,Int]] = null
  var isFarField_face : Rep[Field[Face,Int]] = null
  var insideID : Rep[Field[Face,Int]] = null
  var outsideID : Rep[Field[Face,Int]] = null

  var faceCenter : Rep[Field[Face,Vec[_3,Double]]] = null
  var faceArea : Rep[Field[Face,Double]] = null

  var Phi : Rep[Field[Cell,Double]] = null
  var Flux : Rep[Field[Cell,Double]] = null

  //some geometry fields
  var face_centroid : Rep[Field[Face,Vec[_3,Double]]] = null
  var face_area : Rep[Field[Face,Double]] = null
  var face_normal : Rep[Field[Face,Vec[_3,Double]]] = null
  var face_unit_normal : Rep[Field[Face,Vec[_3,Double]]] = null

  var cell_centroid : Rep[Field[Cell,Vec[_3,Double]]] = null
  var cell_volume : Rep[Field[Cell,Double]] = null
 
  //some HACK functions
  def determineInclusions() : Unit = {
    for(f <- interior_set) {
      isInterior_face(f) = 1;
    }
    for(f <- outlet_set) {
      isOutlet_face(f) = 1;
    }
    for(f <- inlet_set) {
      isInlet_face(f) = 1;
    }
    for(f <- far_field_set) {
      isFarField_face(f) = 1;
    }
    for(f <- faces(mesh)) {
      insideID(f) = ID(inside(f))
      outsideID(f) = ID(outside(f))
    }
  }

  //some geometry functions
  def calcFaceCenter(f : Rep[Face]) : Rep[Vec[_3,Double]] = {
    var center = Vec(0.,0.,0.)
    val test = Vec(0.,0.,0.,0.)
    for(v <- vertices(f)) {
      center += position(v)
    }
    center / size(vertices(f))
  }
  def calcCellCenter(c : Rep[Cell]) : Rep[Vec[_3,Double]] = {
    var center = Vec(0.,0.,0.)
    for(v <- vertices(c)) {
      center += position(v)
    }
    center / size(vertices(c))
  }
  def calcFaceGeom(f : Rep[Face]) : Rep[Unit] = {
    val approxCenter = calcFaceCenter(f)
    var normal = Vec(0.,0.,0.)
    for(e <- edgesCCW(f)) {
      val v0 = position(head(e)) - approxCenter
      val v1 = position(tail(e)) - approxCenter
      // TODO
      normal = normal + cross(v1,v0)
    }
    normal = normalize(normal)
    var center = Vec(0.,0.,0.)
    var area = 0.
    for(e <- edgesCCW(f)) {
      val v0 = position(head(e)) - approxCenter
      val v1 = position(tail(e)) - approxCenter
      val tmp_area = dot(normal,cross(v1,v0))
      area += tmp_area
      // TODO
      center = center + ( approxCenter + position(head(e)) + position(tail(e))) * tmp_area
    }  
    face_centroid(f) = center / (area * 3.)
    val farea = area / 2.
    face_area(f) = farea
    face_normal(f) = normal*farea
    face_unit_normal(f) = normal
  }

  def calcCellGeom(c : Rep[Cell]) : Rep[Unit] = {
    val approxCenter = calcCellCenter(c)
    var volume = 0.
    var center = Vec(0.,0.,0.)
    for(f <- faces(c)) {
      val v0 = face_centroid(f) - approxCenter
      for(e <- edgesCCW(towards(f,c))) {
      val v1 = position(head(e)) - approxCenter
      val v2 = position(tail(e)) - approxCenter
      val tetVol = dot(v0,cross(v1,v2))
      volume += tetVol
      // TODO
      center = center + ( approxCenter + face_centroid(f) + position(head(e)) + position(tail(e))) * tetVol
      }
    }
    cell_centroid(c) = center / (volume * 4.)
    cell_volume(c) = volume / 6.
  }
  def phi_sine_function( t : Rep[Double]) : Rep[Double] = {
    return sin(t*2.*MATH_PI.asInstanceOfL[Double]) * 10.
  }
  def normal_pdf(x : Rep[Double]) : Rep[Double] = exp(- x * x / 2.) / sqrt(2. * MATH_PI.asInstanceOfL[Double])
  def main() {
    position = FieldWithLabel[Vertex,Vec[_3,Double]]("position")
    interior_set = BoundarySet[Face]("default-interior")
    inlet_set = BoundarySet[Face]("inlet")
    outlet_set = BoundarySet[Face]("outlet")
    far_field_set = BoundarySet[Face]("far_field")

    isInterior_face = FieldWithConst[Face,Int](0)
    isOutlet_face = FieldWithConst[Face,Int](0)
    isInlet_face = FieldWithConst[Face,Int](0)
    isFarField_face = FieldWithConst[Face,Int](0)
    insideID = FieldWithConst[Face,Int](0)
    outsideID = FieldWithConst[Face,Int](0)

    faceCenter = FieldWithConst[Face,Vec[_3,Double]](Vec(0.,0.,0.))
    faceArea = FieldWithConst[Face,Double](0.)

    Phi = FieldWithConst[Cell,Double](0.)
    Flux = FieldWithConst[Cell,Double](0.)

  //some geometry fields
    face_centroid = FieldWithConst[Face,Vec[_3,Double]](Vec(0.,0.,0.))
    face_area = FieldWithConst[Face,Double](0.)
    face_normal = FieldWithConst[Face,Vec[_3,Double]](Vec(0.,0.,0.))
    face_unit_normal = FieldWithConst[Face,Vec[_3,Double]](Vec(0.,0.,0.))

    cell_centroid = FieldWithConst[Cell,Vec[_3,Double]](Vec(0.,0.,0.))
    cell_volume = FieldWithConst[Cell,Double](0.)
  
    determineInclusions()         // Initialize HACK

    val globalVelocity = Vec(1.,0.,0.)
    //initialize geometry fields
    for(f <- faces(mesh)) {
      if(ID(outside(f)) < ID(inside(f))) {
      calcFaceGeom(flip(f))
      } else {
      calcFaceGeom(f)
      }
    }
    for(c <- cells(mesh)) {
      calcCellGeom(c)
    }
    
    var ll = Vec(-1.,-1.,-1.) //this needs to become a reduce...
    var ur = Vec(1.,1.,1.) //probably hard-code it for now?
    for(v <- vertices(mesh)) {
      ll = ll min position(v)
      ur = ur max position(v)
    }
    val mesh_center = Vec(0.5f,0.25f,0.25f) //hardcoded for the test mesh
    for(c <- cells(mesh)) {
      val center = cell_centroid(c)
      val x = normal_pdf(center.x - mesh_center.x)
      val y = normal_pdf(center.y - mesh_center.y)
      val z = normal_pdf(center.z - mesh_center.z)
      Phi(c) = x * y * z
    }
      for(f <- faces(mesh)) {
      Print(ID(f),"FaceArea: ",face_area(f),"normal: ",face_unit_normal(f),"face_centroid",face_centroid(f))
    }
    for(c <- cells(mesh)) {
      Print("c: ",ID(c)," ",cell_volume(c)," ",cell_centroid(c))
    }
    val deltat = .015
    var t = 0.
    for(c <- cells(mesh)) {
        Print("before cell number: ",ID(c)," -> phi value: ",Phi(c))
    }
    while(t < 0.01f) {
      
      for(c <- cells(mesh)){
        for(f <- faces(c)){		
          if(isInterior_face(f) > 0){
            val normal = face_unit_normal(f)
            val vDotN = dot(globalVelocity,normal)
            val area = face_area(f)
            var flux = 0.
            val cell = if(vDotN >= 0.) inside(f) else outside(f)
            
            flux = area * vDotN * Phi(cell)
            if(ID(c) == insideID(f))
              Flux(c) -= flux
            if(ID(c) == outsideID(f))
              Flux(c) += flux
          }
          
          if(isOutlet_face(f) > 0){
            val normal = face_unit_normal(f)
            if(ID(outside(f)) == 0 && ID(c) == insideID(f))
              Flux(c) -= face_area(f) * dot(normal,globalVelocity) * Phi(c)
            else if(ID(c) == outsideID(f))
              Flux(c) -= face_area(f) * dot(-normal,globalVelocity) * Phi(c)
          }
          if(isInlet_face(f) > 0){
            if(ID(c) == outsideID(f)){
              val area = face_area(f)
              val vDotN = dot(globalVelocity,face_unit_normal(f))
              Flux(c) += area * vDotN * phi_sine_function(t)
            }
          }
          if(isFarField_face(f) > 0){
            val normal = face_unit_normal(f)
            
            if(ID(outside(f)) == 0 && ID(c) == insideID(f))
              Flux(c) -= dot(normal,globalVelocity) * face_area(f) * Phi(c)
            else if(ID(c) == outsideID(f))
              Flux(c) -= dot(-normal,globalVelocity) * face_area(f) * Phi(c)
          }
    
        }
      }
      for(c <- cells(mesh)) {
        Phi(c) += deltat * Flux(c) / cell_volume(c)
      }
      for(c <- cells(mesh)) {
        Flux(c) = 0.
      }
      t += deltat
    }
    for(c <- cells(mesh)) {
        Print(ID(c), " ",Phi(c))
    }
  }
}
